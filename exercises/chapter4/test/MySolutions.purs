module Test.MySolutions where

import Prelude

import Data.Array (head, tail, foldl, filter, length, (..), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Control.MonadZero (guard)
import Test.Examples (factors)

isEven :: Int -> Boolean
isEven n
  | n == 0 = true
  | n < 0 = false
  | otherwise = isEven(n - 2)

countEven' :: Array Int -> Int
countEven' arr = sumEven arr 0
  where 
    sumEven :: Array Int -> Int -> Int
    sumEven [] ct = ct
    sumEven arr' count = sumEven (fromMaybe [] $ tail arr') $ addIfEven (head arr') count
      where
        addIfEven :: Maybe Int -> Int -> Int
        addIfEven elem ct
          | Just x <- elem, isEven x = 1 + ct
          | otherwise = ct

countEven :: Array Int -> Int
countEven arr = foldl (\e1 e2 -> addIfEven e1 e2) 0 arr
  where
    addIfEven :: Int -> Int -> Int
    addIfEven ct elem
      | isEven elem = 1 + ct
      | otherwise = ct

squared :: Array Number -> Array Number
squared arr = (\n -> n * n) <$> arr

keepNonNegative :: Array Number -> Array Number
keepNonNegative arr = filter (\n -> n >= 0.0) arr

infix 8 filter as <$?>
keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite arr = (\n -> n >= 0.0) <$?> arr

isPrime :: Int -> Boolean
isPrime n = factors n == [[1, n]]

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct a1 a2 = do
  i <- a1
  j <- a2
  pure [i, j]

triples :: Int -> Array (Array Int)
triples n = do
  i <- 1..n
  j <- i..n
  k <- j..n
  guard $ i * i + j * j == k * k
  pure [i, j, k]


factorizations :: Int -> Array Int
factorizations num = findFactors num 2 []
  where
    findFactors :: Int -> Int -> Array Int -> Array Int
    findFactors n curr factors
      | n `mod` curr == 0 = findFactors (n / curr) (curr + 1) (curr : factors)
      | curr >= n = factors
      | otherwise = findFactors n (curr + 1) factors