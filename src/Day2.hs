{-# LANGUAGE TypeApplications #-}

module Day2 (day2_1, day2_2) where

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as C

getPws = C.lines <$> C.readFile "day2_input.txt"

data Req = Req
  { min :: Int,
    max :: Int,
    c :: Char
  }
  deriving (Eq, Show)

validatePw :: Req -> C.ByteString -> Bool
validatePw (Req mn mx c) pw = go 0 (C.unpack pw)
  where
    go n [] = if n >= mn && n <= mx then True else False
    go n (p : ps)
      | n > mx = False
      | p == c = go (n + 1) ps
      | otherwise = go n ps

pwReader = do
  mn <- (read @Int . C.unpack) <$> takeWhile1 isDigit
  char '-'
  mx <- (read @Int . C.unpack) <$> takeWhile1 isDigit
  skipSpace
  c <- anyChar
  char ':'
  skipSpace
  pw <- takeByteString
  return $ (Req mn mx c, pw)

countValid = foldr f 0
  where
    f pw n = case parseOnly pwReader pw of
      Left s -> error s -- gross but w/e
      Right v -> if uncurry validatePw v then n + 1 else n

day2_1 = countValid <$> getPws

validate2 :: Req -> C.ByteString -> Bool
validate2 (Req p1 p2 c) pw = go False $ zipWith (,) [1 ..] (C.unpack pw)
  where
    go v [] = v
    go False ((pos, pc) : xs)
      | pc == c && (pos == p1 || pos == p2) = go True xs
      | otherwise = go False xs
    go True ((pos, pc) : xs)
      | pc == c && (pos == p1 || pos == p2) = False
      | otherwise = go True xs

countValid2 = foldr f 0
  where
    f pw n = case parseOnly pwReader pw of
      Left s -> error s
      Right v -> if uncurry validate2 v then n + 1 else n

day2_2 = countValid2 <$> getPws
