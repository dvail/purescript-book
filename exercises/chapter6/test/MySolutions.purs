module Test.MySolutions where

import Data.Foldable
import Prelude

import Data.Maybe
import Data.Array (cons)
import Data.Monoid (power)

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

data OneMore f a = OneMore a (f a)

instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
  foldl fn acc (OneMore a foldable) = foldl fn (fn acc a) foldable
  foldr fn acc (OneMore a foldable) = fn a (foldr fn acc foldable)
  foldMap fn (OneMore a foldable) = fn a <> foldMap fn foldable

unsafeMaximum' :: Partial => Array Int -> Int
unsafeMaximum' a = case maximum a of
  Just m -> m

unsafeMaximum :: Partial => Array Int -> Int
unsafeMaximum a = unsafeMax max
  where 
    max = maximum a
    unsafeMax (Just m) = m

class Monoid m <= Action m a where
  act :: m -> a -> a

newtype Multiply = Multiply Int

instance semigroupMultiply :: Semigroup Multiply where
  append (Multiply n) (Multiply m) = Multiply (n * m)

instance monoidMultiply :: Monoid Multiply where
  mempty = Multiply 1

instance repeatAction :: Monoid a => Action Multiply a where
  act (Multiply m) s = power s m
