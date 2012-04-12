module Reactive.Banana.SDL.Types ( EventSource, SDLEventSource (..), WrappedEvent
                                 , TickEvent ) where

import Reactive.Banana as R
import Graphics.UI.SDL as SDL
import Data.Word


type EventSource a = (AddHandler a, a -> IO ())
type WrappedEvent t = R.Event t [SDL.Event]
type TickEvent t = R.Event t Word32
data SDLEventSource = SDLEventSource { getSDLEvent :: EventSource [SDL.Event]
                                     , getTickEvent :: EventSource Word32 }