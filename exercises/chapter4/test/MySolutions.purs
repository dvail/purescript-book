module Test.MySolutions where

import Prelude

import Data.Array (head, tail, foldl)
import Data.Maybe (Maybe(..), fromMaybe)

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