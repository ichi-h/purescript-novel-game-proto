module WebAudio
  ( registerNodes
  , deleteNodes
  , changeVolume_
  , playAudio
  , stopAudio
  ) where

import Prelude

import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either)
import Data.Function.Uncurried (Fn2, Fn7, runFn2, runFn7)
import Data.Maybe (Maybe)
import Effect (Effect)

foreign import registerNodes :: String -> Effect Unit
foreign import deleteNodes :: String -> Effect Unit
foreign import playAudioImpl :: Fn7 String ArrayBuffer Int Int Int Int (Maybe { start :: Int, end :: Int }) (Effect (Either String Unit))
foreign import stopAudioImpl :: Fn2 String Int (Effect (Either String Unit))
foreign import changeVolumeImpl :: Fn2 String Number (Effect (Either String Unit))

playAudio :: String -> ArrayBuffer -> Int -> Int -> Int -> Int -> Maybe { start :: Int, end :: Int } -> Effect (Either String Unit)
playAudio = runFn7 playAudioImpl

stopAudio :: String -> Int -> Effect (Either String Unit)
stopAudio = runFn2 stopAudioImpl

changeVolume_ :: String -> Number -> Effect (Either String Unit)
changeVolume_ = runFn2 changeVolumeImpl
