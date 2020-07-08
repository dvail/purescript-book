module Test.MySolutions where

import Data.Foldable
import Prelude

import Data.Array (cons, foldMap, foldl, foldr)

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

instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty def1 a1) (NonEmpty def2 a2) = (NonEmpty def1 $ a1 <> [def2] <> a2)

instance functorNonEmpty :: Functor NonEmpty where
  map f (NonEmpty def a) = NonEmpty (f def) (f <$> a)
  
data Extended a = Finite a | Infinite

derive instance eqExtended :: (Eq a) => Eq (Extended a)

instance ordExtended :: (Ord a) => Ord (Extended a) where
  compare Infinite Infinite = EQ
  compare _ Infinite = LT
  compare Infinite _ = GT
  compare (Finite a1) (Finite a2) = compare a1 a2

instance foldableNonEmpty :: Foldable NonEmpty where
  foldl fn acc (NonEmpty def arr) = foldl fn acc $ cons def arr
  foldr fn acc (NonEmpty def arr) = foldr fn acc $ cons def arr
  foldMap fn (NonEmpty def arr) = foldMap fn $ cons def arr

