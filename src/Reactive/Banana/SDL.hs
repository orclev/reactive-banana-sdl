module Reactive.Banana.SDL ( module Reactive.Banana.SDL.Types
                           , module Reactive.Banana.SDL.Util
                           , getSDLEventSource, runSDLPump
                           , runCappedSDLPump ) where

import Control.Monad
import Reactive.Banana.SDL.Types
import Reactive.Banana.SDL.Util
import Reactive.Banana as R
import Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Time as SdlTime
import Data.Word

getSDLEventSource :: IO SDLEventSource
getSDLEventSource = SDLEventSource <$> newAddHandler <*> newAddHandler

mainSDLPump :: SDLEventSource -> IO Bool
mainSDLPump es = do
    let esdl = getSDLEvent es
        etick = getTickEvent es
    tick <- SdlTime.getTicks
    me <- collectEvents
    case me of
        Nothing -> return False
        Just e -> do
            fire esdl e
            fire etick tick
            return True

collectEvents :: IO (Maybe [SDL.Event])
collectEvents = do
    e <- pollEvent
    case e of
        Quit -> return Nothing
        NoEvent -> return (Just [])
        otherwise -> liftM (liftM (e:)) collectEvents

runSDLPump :: SDLEventSource -> IO ()
runSDLPump es = whileM (mainSDLPump es)

runCappedSDLPump :: Int -> SDLEventSource -> IO ()
runCappedSDLPump rate es = do
    startTick <- SdlTime.getTicks
    c <- mainSDLPump es
    endTick <- SdlTime.getTicks
    let ticks = fromIntegral (endTick - startTick)
        secsPerFrame = fromIntegral (1000 `div` rate)
    when (ticks < secsPerFrame) $
        delay $ secsPerFrame - ticks
    when c $ runCappedSDLPump rate es
