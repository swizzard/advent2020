{-# LANGUAGE OverloadedStrings #-}

module Day8 (day8_1, day8_2) where

import Control.Applicative ((<|>))
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8 as BC
import qualified Data.Set as S
import qualified Data.Vector as V

data Instruction = Acc Int | Jmp Int | Nop Int deriving (Eq, Ord, Show)

parseN = truncate <$> (A.signed A.double)

parseInstruction :: A.Parser Instruction
parseInstruction = parseAcc <|> parseJmp <|> parseNop
  where
    parseAcc = A.string "acc " >> Acc <$> parseN
    parseJmp = A.string "jmp " >> Jmp <$> parseN
    parseNop = A.string "nop " >> Nop <$> parseN

data S = S {acc :: Int, ix :: Int, seen :: S.Set Int} deriving (Show)

type Program = V.Vector Instruction

input = BC.readFile "day8_input.txt"

handleInst :: S -> Instruction -> S
handleInst (S a i s) (Acc n) = S (a + n) (i + 1) (S.insert i s)
handleInst (S a i s) (Jmp n) = S a (i + n) (S.insert i s)
handleInst (S a i s) (Nop _) = S a (i + 1) (S.insert i s)

step :: Program -> Int
step p = go (S 0 0 S.empty)
  where
    go st@(S a i s) =
      if S.member i s
        then a
        else go $ handleInst st (p V.! i)

getInsts = fmap V.fromList <$> A.parseOnly (parseInstruction `A.sepBy` "\n") <$> input

day8_1 = do
  (Right insts) <- getInsts
  return $ step insts

isNopOrJmp (Acc _) = False
isNopOrJmp _ = True

modInst (Nop n) = Acc n
modInst (Jmp n) = Nop n
modInst inst = inst

modProg p i = V.update p $ V.fromList [(i, modInst $ p V.! i)]

nojIxs = V.map fst . V.filter (isNopOrJmp . snd) . V.indexed

step2 :: Program -> Int
step2 p = go $ V.toList $ V.map (modProg p) $ nojIxs p
  where
    go [] = error "empty"
    go (prg : prgs) = case g (S 0 0 S.empty) prg of
      Just v -> v
      Nothing -> go prgs
    g st@(S a i s) prg
      | i == V.length prg = Just a
      | S.member i s = Nothing
      | otherwise = g (handleInst st (prg V.! i)) prg

day8_2 = do
  (Right insts) <- getInsts
  return $ step2 insts
