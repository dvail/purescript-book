module Main where

import Prelude ((<>), show, Unit)
import Euler (answer)
import Effect.Console (log)
import Effect (Effect)

main :: Effect Unit
main = do
  log ("The answer is " <> show (answer 1000))
