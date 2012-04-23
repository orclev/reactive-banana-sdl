{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, TemplateHaskell #-}
module Reactive.Banana.SDL.Graphics.Util where

import Reactive.Banana.SDL.Graphics.Types

import Reactive.Banana as R
import Graphics.UI.SDL as SDL hiding (flip)
import qualified Graphics.UI.SDL as SDL (flip)
import Graphics.UI.SDL.TTF
import Data.Lens.Common
--import Debug.Trace

over :: Graphic -> Graphic -> Graphic
(Graphic x) `over` (Graphic y) = Graphic $ \surface -> y surface >> x surface

under :: Graphic -> Graphic -> Graphic
under = flip over

emptyG :: Graphic
emptyG = Graphic $ \_ -> return ()

render :: Graphic -> Graphic
render (Graphic x) = Graphic $ \surface -> x surface >> SDL.flip surface

withinBox :: Rect -> Graphic -> GraphicOpt
withinBox r g = \r' ->
    if r `intersect` r' then emptyG else g

overOpt :: GraphicOpt -> GraphicOpt -> GraphicOpt
overOpt g1 g2 = \r -> g1 r `over` g2 r

overUpdate :: GraphicOpt -> GraphicUpdate -> GraphicUpdate
overUpdate g1 (g2,r) = (g1 `overOpt` g2, r)

intersect :: Rect -> Rect -> Bool
intersect r1 r2 = xintersect && yintersect
    where
        xintersect = x1 `between` (x2, w2) || w1 `between` (x2,w2)
        yintersect = y1 `between` (y2,h2) || h1 `between` (y2,h2)
        x1 = rectX r1
        x2 = rectX r2
        y1 = rectY r1
        y2 = rectY r2
        w1 = x1 + rectW r1
        w2 = x2 + rectW r2
        h1 = y1 + rectH r1
        h2 = y2 + rectH r2

between :: Int -> (Int,Int) -> Bool
between x (l,h) = x >= l && x <= h

instance Draw SDL.Surface Mask where
    draw src mask = Graphic $ \dst -> discard $ blitSurface src clip dst offset
        where
            clip = maskClip ^$ mask
            offset = Just $ Rect { rectX = maskX ^$ mask, rectY = maskY ^$ mask, rectW = 0, rectH = 0 }

instance Draw Fill Mask where
    draw fill mask = Graphic $ \dst -> pixel dst >>= \c -> discard $ fillRect dst clip c
        where
            pixel dst = (mapRGB . surfaceGetPixelFormat) dst (colorRed color) (colorGreen color) (colorBlue color)
            clip = fillClip ^$ fill
            color = fillColor ^$ fill

instance Draw Text Mask where
    draw text mask = Graphic $ \dst -> discard $ blitText dst
        where
            blitText dst = do
                txt <- renderTextSolid (textFont ^$ text) (textMsg ^$ text) (textColor ^$ text)
                blitSurface txt clip dst offset
                freeSurface txt
            clip = maskClip ^$ mask
            offset = Just $ Rect { rectX = maskX ^$ mask, rectY = maskY ^$ mask, rectW = 0, rectH = 0 }

discard :: IO a -> IO ()
discard m = m >> return ()

renderGraph :: Behavior t Graphic -> Behavior t Screen -> NetworkDescription t ()
renderGraph bgraph bscreen = do
    egraph <- changes $ render <$> bgraph
    reactimate $ flip paintGraphic <$> bscreen <@> egraph

renderGraphOnEvent :: Behavior t Graphic -> Behavior t Screen -> R.Event t a -> NetworkDescription t ()
renderGraphOnEvent bgraph bscreen event =
    reactimate $ paintGraphic <$> render <$> bgraph <*> bscreen <@ event