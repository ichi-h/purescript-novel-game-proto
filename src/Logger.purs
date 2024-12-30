module Logger where

import Prelude

import Effect (Effect)

foreign import debug :: forall a. a -> Effect Unit
foreign import warn :: forall a. a -> Effect Unit
foreign import error :: forall a. a -> Effect Unit

