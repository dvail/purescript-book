module Test.MySolutions where

import Prelude

import Control.Apply (lift2)
import Data.AddressBook (Address, address)
import Data.AddressBook.Validation (Errors, matches, nonEmpty)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldl, foldr, foldMap)
import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (class Traversable, sequence, traverse)
import Data.Validation.Semigroup (V(..))
import Partial.Unsafe (unsafePartial)

-- Note to reader: Add your solutions to this file
addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe = lift2 (+)

subMaybe :: Maybe Int -> Maybe Int -> Maybe Int
subMaybe = lift2 (-)

mulMaybe :: Maybe Int -> Maybe Int -> Maybe Int
mulMaybe = lift2 (*)

divMaybe :: Maybe Int -> Maybe Int -> Maybe Int
divMaybe = lift2 (/)

addApply :: forall f s. Apply f => Semiring s => f s -> f s -> f s
addApply = lift2 (+)

subApply :: forall f s. Apply f => Ring s => f s -> f s -> f s
subApply = lift2 (-)

mulApply :: forall f s. Apply f => Semiring s => f s -> f s -> f s
mulApply = lift2 (*)

divApply :: forall f s. Apply f => EuclideanRing s => f s -> f s -> f s
divApply = lift2 (/)

combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe (Just app) = Just <$> app 
combineMaybe Nothing = pure Nothing

combineMaybe2 :: forall a f. Applicative f => f a -> f (Maybe a)
combineMaybe2 app = Just <$> app 

stateRegex :: Regex
stateRegex =
  unsafePartial case regex "^\\D{2}$" noFlags of
    Right r -> r

nonEmptyRegex :: Regex
nonEmptyRegex =
  unsafePartial case regex "^.*\\S+.*$" noFlags of
    Right r -> r

validateAddressImproved :: Address -> V Errors Address
validateAddressImproved a =
  address <$> (matches "Street" nonEmptyRegex a.street *> pure a.street)
          <*> (matches "City"   nonEmptyRegex a.city   *> pure a.city)
          <*> (matches "State"  stateRegex    a.state  *> pure a.state)

data Tree a = Leaf | Branch (Tree a) a (Tree a)

instance showTree :: Show a => Show (Tree a) where
  show (Branch left v right) =
    "(Branch " <> show left <> " " <> show v <> " " <> show right <> ")"
  show Leaf = "Leaf"

instance eqTree :: Eq a => Eq (Tree a) where
  eq (Branch l1 v1 r1) (Branch l2 v2 r2) = eq l1 l2 && eq v1 v2 && eq r1 r2
  eq Leaf Leaf = true
  eq _ _ = false

instance functorTree :: Functor Tree where
  map fn (Branch l v r) = Branch (map fn l) (fn v) (map fn r)
  map _ Leaf = Leaf

instance foldableTree :: Foldable Tree where
  foldl _ acc Leaf = acc
  foldl fn acc (Branch l v r) = foldl fn (fn (foldl fn acc l) v) r
  foldr _ acc Leaf = acc
  foldr fn acc (Branch l v r) = foldr fn (fn v (foldr fn acc r)) l
  foldMap fn Leaf = mempty
  foldMap fn (Branch l v r) = foldMap fn l <> fn v <> foldMap fn r

instance traversableTree :: Traversable Tree where
  traverse _ Leaf = pure Leaf
  traverse fn (Branch l v r) = Branch <$> traverse fn l <*> fn v <*> traverse fn r
  sequence Leaf = pure Leaf
  sequence (Branch l v r) = Branch <$> sequence l <*> v <*> sequence r

  {- TODO Create a PR for exercise 2.
  The following naive implementation of `sequence` allows tests to pass:

  sequence :: forall a m t. Applicable a => Traversable t => t (m a) -> m (t a) 
  sequence Leaf = pure Leaf
  sequence (Branch l v r) = pure Leaf

  TODO Add test cases for foldl/foldr
  Additionally I'm not sure that the implementations of foldl and foldr above are correct
  -}

traversePreOrder :: forall a m b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePreOrder _ Leaf = pure Leaf
traversePreOrder fn (Branch l v r) = ado
  v2 <- fn v
  l2 <- traversePreOrder fn l
  r2 <- traversePreOrder fn r
  in Branch l2 v2 r2

traversePostOrder :: forall a m b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePostOrder _ Leaf = pure Leaf
traversePostOrder fn (Branch l v r) = ado
  l2 <- traversePostOrder fn l
  r2 <- traversePostOrder fn r
  v2 <- fn v
  in Branch l2 v2 r2

{-
TODO Figure out how to do this without applicative do notation
-}
traversePostOrder' :: forall a m b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePostOrder' _ Leaf = pure Leaf
traversePostOrder' fn (Branch l v r) = 
  -- Branch <$> traversePostOrder fn l <*> fn v <*> traversePostOrder fn r
  apply (apply (map Branch $ traversePostOrder' fn l) $ fn v) $ traversePostOrder' fn r
  where
    mappedBranch :: forall f a2. Functor f => f (Tree a2) -> f (a2 -> Tree a2 -> Tree a2)
    mappedBranch = map Branch
    {-
    mappedBranchWLeftTrav :: forall f a. Functor f => f a -> f (Tree a -> Tree a)
    mappedBranchWLeftTrav = mappedBranch $ traversePostOrder' fn l
    -}