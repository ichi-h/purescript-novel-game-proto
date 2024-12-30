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

import Control.Monad.State (StateT)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Maybe (Maybe)
import Effect (Effect)

newtype Channel = Channel
  { name :: String
  , volume :: Number
  , source :: ArrayBuffer
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
play _ = do
  pure unit

stop :: StopEvent -> StateT Channel Effect Unit
stop _ = do
  pure unit

changeVolume :: ChangeVolumeEvent -> StateT Channel Effect Unit
changeVolume _ = do
  pure unit
