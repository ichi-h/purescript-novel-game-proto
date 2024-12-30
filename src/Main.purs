module Main where

import Prelude

import Affjax.ResponseFormat as AXRF
import Affjax.Web as AX
import Channel (Channel(..), PlayStatus(..), play, PlayEvent(..))
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

data Action = Increment | Decrement

data State = State
  { count :: Int
  , channel :: Channel
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
  { count: 0
  , channel: Channel { name: "channel", playStatus: Stopped, volume: 1.0, audioBuffer: Nothing }
  }

render :: forall m. State -> H.ComponentHTML Action () m
render (State { count }) =
  HH.div_
    [ HH.button [ HE.onClick \_ -> Decrement ] [ HH.text "-" ]
    , HH.div_ [ HH.text $ show count ]
    , HH.button [ HE.onClick \_ -> Increment ] [ HH.text "+" ]
    ]

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Increment -> do
    liftEffect $ registerNodes "channel"
    response <- liftAff $ AX.get AXRF.arrayBuffer "http://localhost:8080/assets/test.mp3"
    case response of
      Left _ -> do
        H.modify_ \(State state) ->
          State $ state { count = state.count + 1 }
      Right { body } -> do
        H.modify_ \(State state) ->
          State
            { count: state.count + 1
            , channel: Channel $ (\(Channel c) -> c { audioBuffer = Just body }) state.channel
            }
  Decrement -> do
    State state <- H.get
    _ <- liftEffect $ play $ PlayEvent { channel: state.channel, offset: 0, durationMs: 10000000, fadeInMs: 0, fadeOutMs: 0, loop: Nothing }
    H.modify_ \(State s) ->
      State { count: s.count - 1, channel: s.channel }
