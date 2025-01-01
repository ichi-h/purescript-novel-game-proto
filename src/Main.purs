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
import Data.Array as Array
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
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
  | NextAnimation
  | FinishAnimation
  | NoOp

data SentenceAnimation
  = Ready
  | Rendering
  | Showing
  | Finished

derive instance eqSentenceAnimation :: Eq SentenceAnimation

data State = State
  { channel :: Channel
  , sentenceAnimation :: SentenceAnimation
  , sentence :: String
  , sentenceIndex :: Int
  }

speedRate :: Number
speedRate = 0.7

maxShowSec :: Number
maxShowSec = 0.1

sentences :: Array String
sentences =
  [ "吾輩は猫である。名前はまだ無い。"
  , "どこで生れたかとんと見当がつかぬ。"
  , "何でも薄暗いじめじめした所でニャーニャー泣いていた事だけは記憶している。"
  , "吾輩はここで始めて人間というものを見た。"
  , "しかもあとで聞くとそれは書生という人間中で一番獰悪な種族であったそうだ。"
  , "この書生というのは時々我々を捕えて煮て食うという話である。"
  , "しかしその当時は何という考もなかったから別段恐しいとも思わなかった。"
  , "ただ彼の掌に載せられてスーと持ち上げられた時何だかフワフワした感じがあったばかりである。"
  , "掌の上で少し落ちついて書生の顔を見たのがいわゆる人間というものの見始であろう。"
  , "この時妙なものだと思った感じが今でも残っている。"
  , "第一毛をもって装飾されべきはずの顔がつるつるしてまるで薬缶だ。"
  , "その後猫にもだいぶ逢ったがこんな片輪には一度も出会わした事がない。"
  , "のみならず顔の真中があまりに突起している。"
  , "そうしてその穴の中から時々ぷうぷうと煙を吹く。"
  , "どうも咽せぽくて実に弱った。"
  , "これが人間の飲む煙草というものである事はようやくこの頃知った。……"
  , "（Replay）"
  ]

totalTimeSec :: String -> Number
totalTimeSec sentence = maxShowSec * (1.0 - speedRate) * toNumber (length sentence)

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
  , sentence: ""
  , sentenceIndex: -1
  }

render :: forall m. State -> H.ComponentHTML Action () m
render (State { sentenceAnimation, sentence }) =
  HH.div
    [ HP.class_ $ HH.ClassName "w-full h-svh flex items-center justify-center"
    ]
    [ if sentenceAnimation == Ready then
        HH.button
          [ HE.onClick \_ -> Play
          , HP.class_ $ HH.ClassName "w-1/2 h-16 text-white text-4xl rounded-sm shadow-lg flex items-center justify-center border-2 border-white"
          ]
          [ HH.text "Play" ]
      else
        HH.div
          [ HP.class_ $ HH.ClassName "w-full h-full flex items-center justify-center"
          , HE.onClick \_ ->
              case sentenceAnimation of
                Ready -> NextAnimation
                Rendering -> NoOp
                Showing -> FinishAnimation
                Finished -> NextAnimation
          ]
          [ HH.div
              [ HP.class_ $ HH.ClassName "w-full max-w-5xl text-4xl text-white break-all select-none "
              ]
              ( mapWithIndex
                  ( \i c -> HH.span
                      [ HP.style
                          ( case sentenceAnimation of
                              Ready -> ""
                              Rendering -> "opacity: 0;"
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
      , loop: Just { start: 0, end: 7474068 }
      }
    H.modify_ \(State s) -> State $ s { channel = c }
    handleAction NextAnimation

  Stop -> do
    State state <- H.get
    c <- liftEffect $ stop $ StopEvent { channel: state.channel, fadeOutMs: 500 }
    H.modify_ \(State s) -> State (s { channel = c })

  StartAnimation -> do
    State beforeState <- H.get
    H.modify_ \(State s) -> State $ s { sentenceAnimation = Showing }
    H.liftAff $ delay $ Milliseconds $ 1000.0 * totalTimeSec beforeState.sentence
    State nextState <- H.get
    if nextState.sentenceIndex == beforeState.sentenceIndex then
      handleAction FinishAnimation
    else
      pure unit

  NextAnimation -> do
    State state <- H.get
    let nextIndex = if state.sentenceIndex + 1 >= Array.length sentences then 0 else state.sentenceIndex + 1
    H.modify_ \(State s) -> State $ s
      { sentenceAnimation = Rendering
      , sentenceIndex = nextIndex
      , sentence = fromMaybe "" $ Array.index sentences nextIndex
      }
    H.liftAff $ delay $ Milliseconds $ 20.0 -- wait for rendering
    handleAction StartAnimation

  FinishAnimation -> H.modify_ \(State s) -> State $ s { sentenceAnimation = Finished }

  NoOp -> pure unit
