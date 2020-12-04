{-# LANGUAGE OverloadedStrings #-}

module Day4 (day4_1, day4_2) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as C
import qualified Data.Set as S

data FieldName = NBYR | NIYR | NEYR | NHGT | NHCL | NECL | NPID | NCID deriving (Enum, Eq, Ord, Show)

readFieldName :: C.ByteString -> Maybe FieldName
readFieldName "byr" = Just NBYR
readFieldName "iyr" = Just NIYR
readFieldName "eyr" = Just NEYR
readFieldName "hgt" = Just NHGT
readFieldName "hcl" = Just NHCL
readFieldName "ecl" = Just NECL
readFieldName "pid" = Just NPID
readFieldName "cid" = Just NCID
readFieldName _ = Nothing

type FieldNames = S.Set FieldName

required = S.fromAscList $ enumFromTo NBYR NPID

input = C.readFile "day4_input.txt"

parseFieldName :: Parser FieldName
parseFieldName = do
  name <- parseFieldName'
  char ':'
  takeTill isSpace
  return name

parseFieldName' :: Parser FieldName
parseFieldName' = do
  raw <- A.take 3
  case readFieldName raw of
    Nothing -> fail "invalid field name"
    Just f -> return f

newPP = A.endOfLine >> A.endOfLine

parsePP :: Parser FieldNames
parsePP = do
  fields <- parseFieldName `sepBy` space
  return $ S.fromList fields

isValidPP = null . (S.difference required)

day4_1 = do
  rawPps <- input
  let res = parseOnly (parsePP `sepBy` newPP) rawPps
  return $ (length . filter isValidPP) <$> res

newtype BirthYear = BirthYear {getBirthYear :: Int} deriving (Eq, Show)

newtype IssueYear = IssueYear {getIssueYear :: Int} deriving (Eq, Show)

newtype ExpirationYear = ExpirationYear {getExpirationYear :: Int} deriving (Eq, Show)

data HeightUnit = CM | IN deriving (Eq, Show)

newtype HairColor = HairColor {getHairColor :: C.ByteString} deriving (Eq, Show)

data EyeColor = AMB | BLU | BRN | GRY | GRN | HZL | OTH deriving (Eq, Show)

readEyeColor "amb" = Just AMB
readEyeColor "blu" = Just BLU
readEyeColor "brn" = Just BRN
readEyeColor "grn" = Just GRN
readEyeColor "gry" = Just GRY
readEyeColor "hzl" = Just HZL
readEyeColor "oth" = Just OTH
readEyeColor _ = Nothing

newtype Pid = Pid {getPid :: C.ByteString} deriving (Eq, Show)

data Field = BYR BirthYear | IYR IssueYear | EYR ExpirationYear | HGT Int HeightUnit | HCL HairColor | ECL EyeColor | PID Pid | CID deriving (Eq, Show)

getFieldName (BYR _) = NBYR
getFieldName (IYR _) = NIYR
getFieldName (EYR _) = NEYR
getFieldName (HGT _ _) = NHGT
getFieldName (ECL _) = NECL
getFieldName (HCL _) = NHCL
getFieldName (PID _) = NPID
getFieldName CID = NCID

parseBirthYear :: Parser Field
parseBirthYear = do
  string "byr:"
  v <- decimal
  guard $ v >= 1920 && v <= 2002
  return $ BYR (BirthYear v)

parseIssueYear = do
  string "iyr:"
  v <- decimal
  guard $ v >= 2010 && v <= 2020
  return $ IYR (IssueYear v)

parseExpirationYear = do
  string "eyr:"
  v <- decimal
  guard $ v >= 2020 && v <= 2030
  return $ EYR (ExpirationYear v)

parseCm = string "cm" >> (pure CM)

parseIn = string "in" >> (pure IN)

parseHeightUnit = parseCm <|> parseIn

parseHeight = do
  string "hgt:"
  v <- decimal
  unit <- parseHeightUnit
  case unit of
    CM -> do
      guard $ v >= 150 && v <= 193
      return $ HGT v CM
    IN -> do
      guard $ v >= 59 && v <= 76
      return $ HGT v IN

parseHairColor = do
  string "hcl:#"
  c <- A.takeWhile (inClass "a-f0-9")
  return $ HCL (HairColor c)

parseEyeColor = do
  string "ecl:"
  raw <- A.take 3
  case readEyeColor raw of
    Nothing -> fail "invalid eye color"
    (Just c) -> return $ ECL c

parsePid = do
  string "pid:"
  ds <- takeWhile1 isDigit
  guard $ C.length ds == 9
  return $ PID (Pid ds)

parseCid = do
  string "cid:"
  takeTill isSpace
  return $ CID

parseField = choice [parseBirthYear, parseIssueYear, parseExpirationYear, parseHeight, parseHairColor, parseEyeColor, parsePid, parseCid]

parseFields = parseField `sepBy` space

isValidWithFields = isValidPP . S.fromList . fmap getFieldName

tokenize s = h : if C.null t then [] else tokenize (C.drop 2 t) where (h, t) = C.breakSubstring "\n\n" s

day4_2 = do
  rawPps <- tokenize <$> input
  let parsed = traverse (parseOnly parseFields) rawPps
  return $ length . filter isValidWithFields <$> parsed
