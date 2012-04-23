{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances
    , TemplateHaskell, TypeOperators #-}
module Reactive.Banana.SDL.Graphics.Types where

import Reactive.Banana as R
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF
import Data.Lens.Common
import Data.Lens.Template

type Screen = SDL.Surface
newtype Graphic = Graphic { paintGraphic :: Screen -> IO () }
type GraphicOpt = Rect -> Graphic
type GraphicUpdate = (GraphicOpt, Rect)
data Mask = Mask { _maskClip :: Maybe Rect, _maskX :: Int, _maskY :: Int }
data Fill = Fill { _fillClip :: Maybe Rect, _fillColor :: Color }
data Text = Text { _textMsg :: String, _textFont :: Font, _textColor :: Color }

$(makeLenses [''Mask, ''Fill, ''Text])

instance Eq Color where
    (Color r1 g1 b1) == (Color r2 g2 b2) = r1 == r2 && g1 == g2 && b1 == b2

instance Show Color where
    show (Color r g b) = "Color { " ++ show r ++ ", " ++ show g ++ ", " ++ show b ++ " }"

class Draw canvas mask where
    draw :: canvas -> mask -> Graphic