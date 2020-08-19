module Test.MySolutions where

import Prelude

import Data.Array (foldM, head, sort, tail)
import Data.List (List(..), (:))
import Data.Maybe (Maybe)

-- Note to reader: Add your solutions to this file

third :: forall a. Array a -> Maybe a
third arr = do
  woFirst <- tail arr
  woSecond <- tail woFirst
  head woSecond

possibleSums :: Array Int -> Array Int
possibleSums arr = sort $ foldM (\a v -> [a, v + a]) 0 arr

filterM :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM _ Nil = pure Nil
filterM fn (x:xs) = do
  bool <- fn x
  xs' <- filterM fn xs
  if bool
  then pure (x:xs')
  else pure xs'
