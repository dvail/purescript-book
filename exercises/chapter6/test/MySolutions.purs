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

data NonEmpty a = NonEmpty a (Array a)

instance eqNonEmpty :: (Eq a) => Eq (NonEmpty a) where
  eq (NonEmpty def1 a1) (NonEmpty def2 a2) = a1 == a2 && def1 == def2

instance showNonEmpty :: (Show a) => Show (NonEmpty a) where
  show (NonEmpty def a) = show def <> show a
