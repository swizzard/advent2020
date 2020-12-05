module Day5 (day5_1, day5_2) where

import Data.Foldable (foldl')
import Data.Functor ((<&>))
import Data.List (sort)
import qualified Data.Vector as V

type Plane = V.Vector Int

planeWidth = 8

planeLength = 128

plane = V.generate (planeWidth * planeLength) id

data Move = F | B | L | R deriving (Eq, Show)

readMove :: Char -> Move
readMove 'F' = F
readMove 'B' = B
readMove 'L' = L
readMove 'R' = R
readMove _ = error "unknown character"

slicePlane :: Plane -> Move -> Plane
slicePlane p m =
  let (h1, h2) = getHalf p
   in case m of
        F -> h1
        L -> h1
        B -> h2
        R -> h2

getHalf :: Plane -> (Plane, Plane)
getHalf p = let h = V.length p `div` 2 in V.splitAt h p

getSeat :: String -> Int
getSeat ticket = V.head $ foldl' f plane ticket
  where
    f :: Plane -> Char -> Plane
    f p c = slicePlane p (readMove c)

input = readFile "day5_input.txt"

allSeats = input <&> lines <&> (fmap getSeat)

day5_1 = maximum <$> allSeats

-- i had 2??
day5_2 = do
  sorted <- sort <$> allSeats
  let candidates = filter (\(a, b) -> b - a == 2) $ zip sorted (drop 1 sorted)
  return $ fmap (\(_, x) -> x - 1) candidates
