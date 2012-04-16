{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, TemplateHaskell #-}
module Reactive.Banana.SDL.Graphics.Util where

import Reactive.Banana.SDL.Graphics.Types

import Reactive.Banana as R
import Graphics.UI.SDL as SDL hiding (flip)
import qualified Graphics.UI.SDL as SDL (flip)
import Graphics.UI.SDL.TTF
import Data.Lens.Common

over :: Graphic -> Graphic -> Graphic
(Graphic x) `over` (Graphic y) = Graphic $ \surface -> y surface >> x surface

under :: Graphic -> Graphic -> Graphic
under = flip over

empty :: Graphic
empty = Graphic $ \_ -> return ()

render :: Graphic -> Graphic
render (Graphic x) = Graphic $ \surface -> x surface >> SDL.flip surface

instance Draw SDL.Surface Mask where
    draw src mask = Graphic $ \dst -> discard $ blitSurface src clip dst offset
        where
            clip = maskClip mask
            offset = Just $ Rect { rectX = maskX mask, rectY = maskY mask, rectW = 0, rectH = 0 }

instance Draw Widget Mask where
    draw widget mask = Graphic $ \dst -> renderWidget widget dst mask

renderWidget :: Widget -> Surface -> Mask -> IO ()
renderWidget w dst mask = case w of
    Over _ _ -> renderWidget (w ^. topItem) dst mask >> (discard $ renderWidget (w ^. bottomItem) dst mask)
    Fill _ -> pixel dst >>= \c -> discard $ fillRect dst Nothing c
    Image _ _ -> discard $ blitSurface (w ^. imageData) (w ^. imageClip) dst offset
    Text _ _ _ -> discard $ blitText dst
    where
        blitText dst = do
            txt <- renderTextSolid (w ^. textFont) (w ^. textMessage) (w ^. textColor)
            blitSurface txt (maskClip mask) dst offset
            freeSurface txt
        offset = Just $ Rect { rectX = maskX mask, rectY = maskY mask, rectW = 0, rectH = 0 }
        pixel dst = (mapRGB . surfaceGetPixelFormat) dst (colorRed color) (colorGreen color) (colorBlue color)
        Fill color = w

discard :: IO a -> IO ()
discard m = m >> return ()

renderGraph :: Behavior t Graphic -> Behavior t Screen -> NetworkDescription t ()
renderGraph bgraph bscreen = do
    egraph <- changes $ render <$> bgraph
    reactimate $ flip paintGraphic <$> bscreen <@> egraph

renderGraphOnEvent :: Behavior t Graphic -> Behavior t Screen -> R.Event t a -> NetworkDescription t ()
renderGraphOnEvent bgraph bscreen event =
    reactimate $ paintGraphic <$> render <$> bgraph <*> bscreen <@ event