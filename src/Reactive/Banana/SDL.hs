module Reactive.Banana.SDL ( EventSource, SDLEventSource, WrappedEvent, TickEvent
                           , addHandler, fire, getSDLEventSource, sdlEvent
                           , tickEvent, keyEvent, mouseEvent, mouseButtonEvent
                           , filterEq, runSDLPump, runCappedSDLPump ) where

import Control.Monad
import Reactive.Banana as R
import Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Time as SdlTime
import Data.Word

type EventSource a = (AddHandler a, a -> IO ())
type WrappedEvent t = R.Event t [SDL.Event]
type TickEvent t = R.Event t Word32
data SDLEventSource = SDLEventSource { getSDLEvent :: EventSource [SDL.Event]
                                     , getTickEvent :: EventSource Word32 }

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd

getSDLEventSource :: IO SDLEventSource
getSDLEventSource = SDLEventSource <$> newAddHandler <*> newAddHandler

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
            MouseMotion _ _ _ _ -> True
            otherwise -> False

mouseButtonEvent :: WrappedEvent t -> WrappedEvent t
mouseButtonEvent = collect . filterE isButton . spill
    where
        isButton e = case e of
            MouseButtonDown _ _ _ -> True
            MouseButtonUp _ _ _ -> True
            otherwise -> False

filterEq :: Eq a => R.Event t a -> R.Event t a
filterEq = filterJust . fst . mapAccum Nothing . fmap f
    where
        f y (Just x) = if x == y then (Nothing, Just x) else (Just y, Just y)
        f y Nothing  = (Just y, Just y)

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
        otherwise -> collectEvents >>= return . (liftM (e:))

runSDLPump :: SDLEventSource -> IO ()
runSDLPump es = whileM (mainSDLPump es)

whileM :: IO Bool -> IO ()
whileM f = f >>= (\x -> if x then whileM f else return ())

runCappedSDLPump :: Int -> SDLEventSource -> IO ()
runCappedSDLPump rate es = do
    startTick <- SdlTime.getTicks
    c <- mainSDLPump es
    endTick <- SdlTime.getTicks
    let ticks = fromIntegral (endTick - startTick)
        secsPerFrame = fromIntegral (1000 `div` rate)
    when (ticks < secsPerFrame) $ do
        delay $ secsPerFrame - ticks
    if c then runCappedSDLPump rate es else return ()
