module Channel
  ( ChangeVolumeEvent(..)
  , Channel
  , PlayEvent(..)
  , StopEvent(..)
  , changeVolume
  , play
  , stop
  ) where

import Prelude

import Control.Monad.State (StateT, get, modify_)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Class (liftEffect)
import WebAudio (playAudio, stopAudio, changeVolume_)

data PlayStatus = Playing | Stopped

data Channel = Channel
  { name :: String
  , playStatus :: PlayStatus
  , volume :: Number
  , audioBuffer :: ArrayBuffer
  }

newtype PlayEvent = PlayEvent
  { offset :: Int
  , durationMs :: Int
  , fadeInMs :: Int
  , fadeOutMs :: Int
  , loop :: Maybe { start :: Int, end :: Int }
  }

newtype StopEvent = StopEvent
  { fadeOutMs :: Int
  }

newtype ChangeVolumeEvent = ChangeVolumeEvent Number

play :: PlayEvent -> StateT Channel Effect Unit
play (PlayEvent { offset, durationMs, fadeInMs, fadeOutMs, loop }) = do
  Channel channel <- get
  res <- liftEffect $ playAudio channel.name channel.audioBuffer offset durationMs fadeInMs fadeOutMs loop
  case res of
    Left _ -> pure unit
    Right _ -> modify_ \(Channel c) -> Channel c { playStatus = Playing }

stop :: StopEvent -> StateT Channel Effect Unit
stop (StopEvent { fadeOutMs }) = do
  Channel channel <- get
  res <- liftEffect $ stopAudio channel.name fadeOutMs
  case res of
    Left _ -> pure unit
    Right _ -> modify_ \(Channel c) -> Channel c { playStatus = Stopped }

changeVolume :: ChangeVolumeEvent -> StateT Channel Effect Unit
changeVolume (ChangeVolumeEvent volume) = do
  Channel channel <- get
  res <- liftEffect $ changeVolume_ channel.name volume
  case res of
    Left _ -> pure unit
    Right _ -> modify_ \(Channel c) -> Channel c { volume = volume }
