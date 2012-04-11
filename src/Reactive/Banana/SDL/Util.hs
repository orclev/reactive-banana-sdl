module Reactive.Banana.SDL.Util ( addHandler, fire, sdlEvent, tickEvent
                                , keyEvent, mouseEvent, mouseButtonEvent
                                , filterEq, keyFilter, whileM ) where

import Reactive.Banana as R
import Graphics.UI.SDL as SDL
import Reactive.Banana.SDL.Types
import Control.Monad (when)

whileM :: IO Bool -> IO ()
whileM f = f >>= (\x -> when x $ whileM f )

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd

sdlEvent :: SDLEventSource -> NetworkDescription t (WrappedEvent t)
sdlEvent = fromAddHandler . addHandler . getSDLEvent

tickEvent :: SDLEventSource -> NetworkDescription t (TickEvent t)
tickEvent = fromAddHandler . addHandler . getTickEvent

keyEvent :: WrappedEvent t -> WrappedEvent t
keyEvent = collect . filterE isKey . spill
    where
        isKey e = case e of
            KeyUp _ -> True
            KeyDown _ -> True
            otherwise -> False

mouseEvent :: WrappedEvent t -> WrappedEvent t
mouseEvent esdl = mouseMotion `union` mouseButtonEvent esdl
    where
        mouseMotion = collect . filterE isMotion $ spill esdl
        isMotion e = case e of
            MouseMotion {} -> True
            otherwise -> False

mouseButtonEvent :: WrappedEvent t -> WrappedEvent t
mouseButtonEvent = collect . filterE isButton . spill
    where
        isButton e = case e of
            MouseButtonDown {} -> True
            MouseButtonUp {} -> True
            otherwise -> False

filterEq :: Eq a => R.Event t a -> R.Event t a
filterEq = filterJust . fst . mapAccum Nothing . fmap f
    where
        f y (Just x) = if x == y then (Nothing, Just x) else (Just y, Just y)
        f y Nothing  = (Just y, Just y)

keyFilter :: SDL.SDLKey -> SDL.Event -> Bool
keyFilter k (KeyDown (Keysym k' _ _)) | k == k' = True
keyFilter _ _ = False