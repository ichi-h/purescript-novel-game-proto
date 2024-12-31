module Main
  ( Action(..)
  , AppError(..)
  , SentenceAnimation(..)
  , State(..)
  , handleAction
  , initialState
  , main
  , mapError
  , render
  ) where

import Prelude

import Affjax.ResponseFormat as AXRF
import Affjax.Web as AX
import Channel (Channel(..), PlayEvent(..), PlayStatus(..), StopEvent(..), play, stop)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Array (mapWithIndex)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.String (length)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import WebAudio (registerNodes)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

data Action
  = Setup
  | Play
  | Stop
  | StartAnimation
  | FinishAnimation
  | RestartAnimation

data SentenceAnimation
  = Ready
  | Showing
  | Finished

derive instance eqSentenceAnimation :: Eq SentenceAnimation

data State = State
  { channel :: Channel
  , sentenceAnimation :: SentenceAnimation
  }

speedRate :: Number
speedRate = 0.95

maxShowSec :: Number
maxShowSec = 0.1

sentence :: String
sentence = "吾輩は猫である。名前はまだ無い。どこで生れたかとんと見当がつかぬ。何でも薄暗いじめじめした所でニャーニャー泣いていた事だけは記憶している。吾輩はここで始めて人間というものを見た。"

totalTimeSec :: Number
totalTimeSec = maxShowSec * (1.0 - speedRate) * toNumber (length sentence)

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Setup }
    }

initialState :: forall i. i -> State
initialState _ = State
  { channel: Channel
      { name: "channel"
      , playStatus: Stopped
      , volume: 1.0
      , audioBuffer: Nothing
      }
  , sentenceAnimation: Ready
  }

render :: forall m. State -> H.ComponentHTML Action () m
render (State { sentenceAnimation }) =
  HH.div_
    [ HH.button [ HE.onClick \_ -> Play ] [ HH.text "play" ]
    , HH.button [ HE.onClick \_ -> Stop ] [ HH.text "stop" ]
    , HH.div
        [ HP.style "width: 400px; height: 200px; border:1px solid black;"
        , HE.onClick \_ ->
            case sentenceAnimation of
              Ready -> StartAnimation
              Showing -> FinishAnimation
              Finished -> RestartAnimation
        ]
        ( mapWithIndex
            ( \i c -> HH.span
                [ HP.style
                    ( case sentenceAnimation of
                        Ready -> "opacity: 0;"
                        Showing ->
                          "transition-timing-function: ease-in; "
                            <> (if speedRate /= 1.0 then "transition-duration: 0.05s;" else "")
                            <> " transition-delay: "
                            <> (show $ maxShowSec * (1.0 - speedRate) * toNumber i)
                            <> "s; opacity: 1;"
                        Finished -> "opacity: 1;"
                    )
                ]
                [ HH.text (fromCharArray [ c ]) ]
            ) $ toCharArray sentence
        )
    ]

data AppError
  = RegisterError
  | FetchError

mapError :: forall a b. AppError -> Either a b -> Either AppError b
mapError _ (Right b) = Right b
mapError a _ = Left a

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Setup -> void $ runExceptT do
    _ <- ExceptT $ map (mapError RegisterError) $ liftEffect $ registerNodes "channel"
    response <- ExceptT
      $ map (mapError FetchError)
      $ liftAff
      $ AX.get AXRF.arrayBuffer "http://localhost:8080/assets/test.mp3"
    H.modify_ \(State s) -> State (s { channel = updateAudioBuffer s.channel response.body })
    where
    updateAudioBuffer (Channel c) buffer = Channel c { audioBuffer = Just buffer }

  Play -> do
    State state <- H.get
    c <- liftEffect $ play $ PlayEvent
      { channel: state.channel
      , delayMs: 0
      , offsetMs: 0
      , fadeInMs: 0
      , fadeOutMs: 0
      , loop: Nothing
      }
    H.modify_ \(State s) -> State { channel: c, sentenceAnimation: s.sentenceAnimation }

  Stop -> do
    State state <- H.get
    c <- liftEffect $ stop $ StopEvent { channel: state.channel, fadeOutMs: 500 }
    H.modify_ \(State s) -> State (s { channel = c })

  StartAnimation -> do
    H.modify_ \(State s) -> State $ s { sentenceAnimation = Showing }
    void $ H.fork $ do
      H.liftAff $ delay $ Milliseconds $ totalTimeSec * 1000.0
      handleAction FinishAnimation

  FinishAnimation -> H.modify_ \(State s) -> State $ s { sentenceAnimation = Finished }

  RestartAnimation -> H.modify_ \(State s) -> State $ s { sentenceAnimation = Ready }
