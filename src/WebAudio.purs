module WebAudio
  ( registerNodes
  , deleteNodes
  , changeVolume_
  , playAudio
  , stopAudio
  ) where

import Prelude

import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Function.Uncurried (Fn2, Fn6, runFn2, runFn6)
import Data.Maybe (Maybe)
import Effect (Effect)

foreign import registerNodes :: String -> Effect Unit
foreign import deleteNodes :: String -> Effect Unit
foreign import playAudioImpl :: Fn6 String ArrayBuffer Int Int Int (Maybe { start :: Int, end :: Int }) (Effect Boolean)
foreign import stopAudioImpl :: Fn2 String Int (Effect Boolean)
foreign import changeVolumeImpl :: Fn2 String Number (Effect Boolean)

playAudio :: String -> ArrayBuffer -> Int -> Int -> Int -> Maybe { start :: Int, end :: Int } -> Effect Boolean
playAudio = runFn6 playAudioImpl

stopAudio :: String -> Int -> Effect Boolean
stopAudio = runFn2 stopAudioImpl

changeVolume_ :: String -> Number -> Effect Boolean
changeVolume_ = runFn2 changeVolumeImpl
