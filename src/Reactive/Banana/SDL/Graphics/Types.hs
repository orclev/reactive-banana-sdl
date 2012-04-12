{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
module Reactive.Banana.SDL.Graphics.Types where

import Reactive.Banana as R
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF

type Screen = SDL.Surface
newtype Graphic = Graphic { paintGraphic :: Screen -> IO () }
data Mask = M { maskClip :: Maybe Rect, maskX :: Int, maskY :: Int }
data Fill = F { fillClip :: Maybe Rect, fillColor :: Color }
data Text = T { textMsg :: String, textFont :: Font, textColor :: Color }

class Draw canvas mask where
    draw :: canvas -> mask -> Graphic