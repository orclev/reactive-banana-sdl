module Reactive.Banana.SDL where

import Reactive.Banana as R
import Graphics.UI.SDL as SDL

type EventSource a = (AddHandler a, a -> IO ())
type SDLEventSource = EventSource SDL.Event

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd

setupEventHandler :: IO (EventSource SDL.Event)
setupEventHandler = newAddHandler

getSDLEvent :: EventSource SDL.Event -> NetworkDescription t (R.Event t SDL.Event)
getSDLEvent es = fromAddHandler $ addHandler es

filterKeyEvents esdl = keyDown `union` keyUp
    where
        keyDown = filterE isKey esdl
        keyUp = filterE isKey esdl
        isKey e = case e of
            KeyUp _ -> True
            KeyDown _ -> True
            otherwise -> False

runSDLPump :: EventSource SDL.Event -> IO ()
runSDLPump es = do
    e <- pollEvent
    case e of
        Quit -> return ()
        otherwise -> fire es e
    runSDLPump es
