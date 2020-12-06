module Day6 (day6_1, day6_2) where

import qualified Data.Set as S

input :: IO String
input = readFile "day6_input.txt"

groups :: [String] -> [[String]]
groups = foldr f [[]]
  where
    f "" acc = [] : acc
    f s (h : t) = (s : h) : t

responses :: [[String]] -> [S.Set Char]
responses = fmap (S.fromList . mconcat)

respCount :: [S.Set Char] -> Int
respCount rs = sum $ length <$> rs

day6_1 :: IO Int
day6_1 = respCount . responses . groups . lines <$> input

indivResponses :: [[String]] -> [[S.Set Char]]
indivResponses = (fmap . fmap) S.fromList

allGroup :: [[S.Set Char]] -> [S.Set Char]
allGroup = fmap (foldr1 S.intersection)

day6_2 :: IO Int
day6_2 = sum <$> (fmap length . allGroup . indivResponses . groups . lines) <$> input
