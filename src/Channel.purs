module Channel
  ( ChangeVolumeEvent(..)
  , Channel(..)
  , PlayEvent(..)
  , StopEvent(..)
  , PlayStatus(..)
  , changeVolume
  , play
  , stop
  ) where

import Prelude

import Data.ArrayBuffer.ArrayBuffer (empty)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Logger as Logger
import WebAudio (playAudio, stopAudio, changeVolume_)

data PlayStatus = Playing | Stopped

data Channel = Channel
  { name :: String
  , playStatus :: PlayStatus
  , volume :: Number
  , audioBuffer :: Maybe ArrayBuffer
  }

data PlayEvent = PlayEvent
  { channel :: Channel
  , offsetMs :: Int
  , fadeInMs :: Int
  , fadeOutMs :: Int
  , loop :: Maybe { start :: Int, end :: Int }
  }

data StopEvent = StopEvent
  { channel :: Channel
  , fadeOutMs :: Int
  }

data ChangeVolumeEvent = ChangeVolumeEvent
  { channel :: Channel
  , volume :: Number
  }

play :: PlayEvent -> Effect Channel
play (PlayEvent { channel: Channel c, offsetMs, fadeInMs, fadeOutMs, loop }) = do
  buffer <- case c.audioBuffer of
    Just b -> pure b
    Nothing -> do
      b <- empty 0
      pure b
  res <- liftEffect $ playAudio c.name buffer offsetMs fadeInMs fadeOutMs loop
  case res of
    false -> do
      Logger.error "Failed to play audio"
      pure $ Channel c
    true -> pure $ Channel $ c { playStatus = Playing }

stop :: StopEvent -> Effect Channel
stop (StopEvent { channel: Channel c, fadeOutMs }) = do
  res <- stopAudio c.name fadeOutMs
  case res of
    false -> do
      Logger.error "Failed to stop audio"
      pure $ Channel c
    true -> pure $ Channel $ c { playStatus = Stopped }

changeVolume :: ChangeVolumeEvent -> Effect Channel
changeVolume (ChangeVolumeEvent { channel: Channel c, volume }) = do
  res <- changeVolume_ c.name volume
  case res of
    false -> do
      Logger.error "Failed to change volume"
      pure $ Channel c
    true -> pure $ Channel $ c { volume = volume }
