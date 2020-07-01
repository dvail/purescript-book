module Test.MySolutions where

import Prelude

import Control.MonadZero (guard)
import Data.Array (head, last, init, null, tail, foldl, foldr, filter, (..), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Path (Path(Directory, File), isDirectory, ls, filename)
import Data.String (Pattern(..), split, joinWith, stripSuffix)
import Data.Tuple (Tuple(..), snd)
import Effect.Exception (name)
import Test.Examples (factors)
import Test.QuickCheck.Gen (sample)

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

allTrue :: Array Boolean -> Boolean
allTrue a = foldl (&&) true a

{- 1, 1, 2, 3, 5, 8, 13, 21, 34, 55 -}
{- peeking.... -}
fibTailRec :: Int -> Int
fibTailRec num = fib' num 0 0 1
  where
  fib' :: Int -> Int -> Int -> Int -> Int
  fib' limit count n1 n2
    | limit == count = n1 + n2
    | otherwise = fib' limit (count + 1) (n1 + n2) n1

reverse :: forall a. Array a -> Array a
reverse = foldl (\xs x -> [x] <> xs) []

onlyFiles :: Path -> Array Path
onlyFiles path = accumFiles path []
  where
    accumFiles :: Path -> Array Path -> Array Path
    accumFiles path' files 
      | isDirectory path' = foldr (\p fs -> accumFiles p fs) files (ls path')
      | otherwise = path' : files

type FileSize = (Tuple String Int)

type LargestSmallest = 
  { largest :: Maybe FileSize
  , smallest :: Maybe FileSize
  }

updateLargestSmallest :: String -> Int -> LargestSmallest -> LargestSmallest
updateLargestSmallest filename size {largest: l, smallest: s} = 
  { largest: Just (updateTuple (>) l)
  , smallest: Just (updateTuple (<) s)
  }
  where
    updateTuple :: (forall a. Ord a => a -> a -> Boolean) -> Maybe FileSize -> FileSize
    updateTuple comparator Nothing = (Tuple filename size)
    updateTuple comparator (Just tuple)
      | size `comparator` snd tuple = (Tuple filename size)
      | otherwise = tuple

largestSmallest :: Path -> LargestSmallest
largestSmallest p = largestSmallest' p { largest: Nothing, smallest: Nothing }
  where 
    largestSmallest' :: Path -> LargestSmallest -> LargestSmallest
    largestSmallest' (File name size) lgSm = updateLargestSmallest name size lgSm
    largestSmallest' path lgSm
      | null $ ls path = lgSm
      | otherwise = foldr largestSmallest' lgSm $ ls path

-- | NOTE - this does not account for a duplicate names throughout the fs
whereIs :: Path -> String -> Maybe String
whereIs (File name _) search = stripSuffix (Pattern search) name
whereIs path search = whereIsWithResult path Nothing
  where
    whereIsWithResult subpath (Just r) = Just $ filename subpath
    whereIsWithResult subpath Nothing = foldr (\p r -> case r of 
      Just result -> Just result
      -- | TODO ensure the search stops once the file is found
      _ -> whereIs p search
    ) Nothing $ ls subpath

whereIsDo :: Path -> String -> Maybe String
whereIsDo path search = Just search
-- | TODO impl
