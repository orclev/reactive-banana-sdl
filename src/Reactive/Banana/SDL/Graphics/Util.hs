{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
module Reactive.Banana.SDL.Graphics.Util where

import Reactive.Banana.SDL.Graphics.Types

import Reactive.Banana as R
import Graphics.UI.SDL as SDL hiding (flip)
import qualified Graphics.UI.SDL as SDL (flip)
import Graphics.UI.SDL.TTF

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

instance Draw Fill Mask where
    draw fill mask = Graphic $ \dst -> pixel dst >>= \c -> discard $ fillRect dst clip c
        where
            pixel dst = (mapRGB . surfaceGetPixelFormat) dst (colorRed color) (colorGreen color) (colorBlue color)
            clip = fillClip fill
            color = fillColor fill

instance Draw Text Mask where
    draw text mask = Graphic $ \dst -> discard $ blitText dst
        where
            blitText dst = do
                txt <- renderTextSolid (textFont text) (textMsg text) (textColor text)
                blitSurface txt clip dst offset
                freeSurface txt
            clip = maskClip mask
            offset = Just $ Rect { rectX = maskX mask, rectY = maskY mask, rectW = 0, rectH = 0 }

discard :: IO a -> IO ()
discard m = m >> return ()

renderGraph :: Behavior t Graphic -> Behavior t Screen -> NetworkDescription t ()
renderGraph bgraph bscreen = do
    egraph <- changes bgraph
    reactimate $ flip paintGraphic <$> bscreen <@> egraph

renderGraphOnEvent :: Behavior t Graphic -> Behavior t Screen -> R.Event t a -> NetworkDescription t ()
renderGraphOnEvent bgraph bscreen event =
    reactimate $ paintGraphic <$> bgraph <*> bscreen <@ event