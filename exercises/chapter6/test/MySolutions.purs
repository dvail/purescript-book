module Test.MySolutions where

import Prelude

-- Note to reader: Add your solutions to this file

newtype Complex = Complex
  { real :: Number
  , imaginary :: Number
  }

instance showComplex :: Show Complex where
  show (Complex c)
    | c.imaginary > 0.0 = show c.real <> "+" <> show c.imaginary <> "i"
    | otherwise = show c.real <> show c.imaginary <> "i"

derive instance eqComplex :: Eq Complex