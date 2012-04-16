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
data Mask = M { maskClip :: Maybe Rect, maskX :: Int, maskY :: Int }

instance Eq Color where
    (Color r1 g1 b1) == (Color r2 g2 b2) = r1 == r2 && g1 == g2 && b1 == b2

instance Show Color where
    show (Color r g b) = "Color { " ++ show r ++ ", " ++ show g ++ ", " ++ show b ++ " }"

data Widget = FileImage { _imagePath :: FilePath } |
    Image { _imageData :: SDL.Surface, _imageClip :: Maybe Rect } | Fill { _fillColor :: Color } |
    Text { _textColor :: Color, _textMessage :: String, _textFont :: Font } |
    Over { _topItem :: Widget, _bottomItem :: Widget }
    deriving (Eq, Show)

$(makeLenses [''Widget])

class Draw canvas mask where
    draw :: canvas -> mask -> Graphic