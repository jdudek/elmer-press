module Util.Effects where

import Effects exposing (Effects)

noFx : m -> (m, Effects a)
noFx model =
  (model, Effects.none)

withFx : (Effects a) -> m -> (m, Effects a)
withFx effects model =
  (model, effects)
