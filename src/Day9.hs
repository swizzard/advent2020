{-# LANGUAGE TypeApplications #-}

module Day9 (day9_1, day9_2) where

import Control.Monad (join)
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import Data.Vector.Split (divvy)

input :: IO (V.Vector Int)
input = V.fromList . fmap (fst . fromJust . BC.readInt) . BC.lines <$> BC.readFile "day9_input.txt"

preludeLength = 25

invalidNum :: [Int] -> Int -> Bool
invalidNum ns n = null $ [(a, b) | a <- ns, b <- ns, a /= b, a + b == n]

slices l v = V.toList <$> divvy l 1 v

firstInvalid :: Int -> V.Vector Int -> Int
firstInvalid l v =
  let candidates = zip (slices l v) (V.toList $ V.drop l v)
   in go candidates
  where
    go [] = error "oh no"
    go (c : cs)
      | uncurry invalidNum c = snd c
      | otherwise = go cs

day9_1 = firstInvalid preludeLength <$> input

subVecs :: V.Vector Int -> [V.Vector Int]
subVecs v = join $ fmap (\n -> divvy n 1 v) [1 .. V.length v]

getContiguousSet :: V.Vector Int -> Int -> V.Vector Int
getContiguousSet ns tgt = head $ filter (\n -> V.length n > 1 && V.sum n == tgt) $ subVecs ns

sumMinMax :: V.Vector Int -> Int
sumMinMax ns = minimum ns + maximum ns

day9_2 = do
  inp <- input
  let tgt = firstInvalid preludeLength inp
  return $ sumMinMax $ getContiguousSet inp tgt
