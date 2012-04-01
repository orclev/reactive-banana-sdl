module Reactive.Banana.SDL where

import Reactive.Banana as R
import Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Time as SdlTime
import Data.Word

type EventSource a = (AddHandler a, a -> IO ())
type SDLEventSource = (EventSource SDL.Event, EventSource Word32)
type WrappedEvent t = R.Event t SDL.Event

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd

getSDLEventSource :: IO SDLEventSource
getSDLEventSource = (,) <$> newAddHandler <*> newAddHandler

sdlEvent :: SDLEventSource -> NetworkDescription t (WrappedEvent t)
sdlEvent = fromAddHandler . addHandler . fst

tickEvent :: SDLEventSource -> NetworkDescription t (R.Event t Word32)
tickEvent = fromAddHandler . addHandler . snd

keyEvent :: WrappedEvent t -> WrappedEvent t
keyEvent = filterE isKey
    where
        isKey e = case e of
            KeyUp _ -> True
            KeyDown _ -> True
            otherwise -> False

mouseEvent :: WrappedEvent t -> WrappedEvent t
mouseEvent esdl = mouseMotion `union` mouseButtonEvent esdl
    where
        mouseMotion = filterE isMotion esdl
        isMotion e = case e of
            MouseMotion _ _ _ _ -> True
            otherwise -> False

mouseButtonEvent :: WrappedEvent t -> WrappedEvent t
mouseButtonEvent = filterE isButton
    where
        isButton e = case e of
            MouseButtonDown _ _ _ -> True
            MouseButtonUp _ _ _ -> True
            otherwise -> False

runSDLPump :: SDLEventSource -> IO ()
runSDLPump es = do
    let esdl = fst es
        etick = snd es
    e <- pollEvent
    tick <- SdlTime.getTicks
    case e of
        Quit -> return ()
        otherwise -> fire esdl e
    fire etick tick
    runSDLPump es
