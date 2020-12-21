{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day7 (day7_1, day7_2) where

import Control.Applicative (many, (<|>))
import Control.Monad
import Control.Monad.Fix (fix)
import Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as BC
import Data.Functor (($>), (<&>))
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as S

input :: IO BC.ByteString
input = BC.readFile "day7_input.txt"

newtype Bag = Bag {getBag :: String} deriving (Eq, Ord, Show)

type ContainedBy = (Bag, Bag)

type ContMap = M.Map Bag [Bag]

parseBag :: Parser Bag
parseBag = do
  adj <- A.takeTill isSpace <&> BC.unpack
  skipSpace
  color <- A.takeTill isSpace <&> BC.unpack
  skipSpace
  string "bag"
  many (char 's')
  return $ Bag $ adj ++ " " ++ color

parseBagCount :: Parser (Maybe (Int, Bag))
parseBagCount = parseNoBags <|> parseNBags
  where
    parseNBags = do
      n <- many digit <&> read
      skipSpace
      b <- parseBag
      return $ Just (n, b)
    parseNoBags = string "no other bags" $> Nothing

parseContainedBy :: Parser [ContainedBy]
parseContainedBy = do
  container <- parseBag
  skipSpace
  string "contain"
  skipSpace
  containees <- parseBagCount `sepBy` ", " <&> catMaybes
  return (containees <&> ((,container) . snd))

parseCbs :: Parser [ContainedBy]
parseCbs = join <$> parseContainedBy `sepBy` ".\n"

toContMap :: [ContainedBy] -> ContMap
toContMap = (M.fromListWith (++)) . fmap (\(r, e) -> (r, [e]))

fl :: ContMap -> Bag -> [Bag]
fl m = fromMaybe [] . flip M.lookup m

getConts cm = go S.empty [Bag "shiny gold"]
  where
    go s [] = s
    go s bs =
      let cs = bs >>= fl cm
          scs = S.fromList cs
       in if null (S.difference scs s) then s else go (S.union s scs) cs

day7_1 = do
  (Right cm) <- fmap toContMap <$> A.parseOnly parseCbs <$> input
  return $ (length $ getConts cm)

parseContains :: Parser (Bag, [(Int, Bag)])
parseContains = do
  container <- parseBag
  skipSpace
  string "contain"
  skipSpace
  containees <- parseBagCount `sepBy` ", " <&> catMaybes
  return (container, containees)

type ContainsMap = M.Map Bag [(Int, Bag)]

parseCs :: Parser ContainsMap
parseCs = M.fromList <$> parseContains `sepBy` ".\n"

lc :: ContainsMap -> (Int, Bag) -> [(Int, Bag)]
lc cm (n, b) =
  let rs = fromMaybe [] $ M.lookup b cm
   in fmap (\(n', b') -> (n' * n, b')) rs

getCs :: ContainsMap -> Int
getCs cm = go 0 [(1, Bag "shiny gold")]
  where
    go :: Int -> [(Int, Bag)] -> Int
    go n [] = n
    go n bs =
      let cs = bs >>= lc cm
       in if null cs then n else let n' = n + sum (fmap fst cs) in go n' cs

day7_2 = do
  (Right cs) <- A.parseOnly parseCs <$> input
  return $ getCs cs
