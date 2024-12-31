module Main where

import Prelude

import Affjax.ResponseFormat as AXRF
import Affjax.Web as AX
import Channel (Channel(..), PlayEvent(..), PlayStatus(..), StopEvent(..), play, stop)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import WebAudio (registerNodes)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

data Action = Setup | Play | Stop

data State = State
  { channel :: Channel
  }

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ = State
  { channel: Channel { name: "channel", playStatus: Stopped, volume: 1.0, audioBuffer: Nothing }
  }

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div_
    [ HH.button [ HE.onClick \_ -> Setup ] [ HH.text "setup" ]
    , HH.button [ HE.onClick \_ -> Play ] [ HH.text "play" ]
    , HH.button [ HE.onClick \_ -> Stop ] [ HH.text "stop" ]
    ]

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Setup -> do
    liftEffect $ registerNodes "channel"
    response <- liftAff $ AX.get AXRF.arrayBuffer "http://localhost:8080/assets/test.mp3"
    case response of
      Left _ -> do
        pure unit
      Right { body } -> do
        H.modify_ \(State state) ->
          State
            { channel: Channel $ (\(Channel c) -> c { audioBuffer = Just body }) state.channel
            }
  Play -> do
    State state <- H.get
    c <- liftEffect $ play $ PlayEvent
      { channel: state.channel
      , offsetMs: 1000
      , fadeInMs: 0
      , fadeOutMs: 500
      , loop: Just { start: 48000 * 5, end: 48000 * 6 }
      }
    H.modify_ \_ -> State { channel: c }

  Stop -> do
    State state <- H.get
    c <- liftEffect $ stop $ StopEvent { channel: state.channel, fadeOutMs: 500 }
    H.modify_ \_ -> State { channel: c }
