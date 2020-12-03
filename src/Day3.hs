module Day3 (day3_1, day3_2) where

import Data.Foldable (foldMap')
import Data.Functor ((<&>))
import Data.Monoid (Product (..))
import qualified Data.Vector as V

data Pos = Open | Tree deriving (Eq, Show)

type Course = V.Vector (V.Vector Pos)

data CourseStep = CourseStep
  { x :: Int,
    y :: Int,
    treeCount :: Int
  }
  deriving (Show)

data CourseInc = CourseInc
  { dx :: Int,
    dy :: Int
  }
  deriving (Show)

readPos :: Char -> Pos
readPos '.' = Open
readPos '#' = Tree
readPos _ = error "unknown position"

lineToRow line = V.fromList $ readPos <$> line

getCourse :: IO Course
getCourse = do
  raws <- lines <$> readFile "day3_input.txt"
  return $ V.fromList $ lineToRow <$> raws

coursePos :: Course -> Int -> Int -> Maybe Pos
coursePos c x y = c V.!? y <&> \row -> row V.! (x `mod` V.length row)

countTrees :: Course -> CourseInc -> Int
countTrees c (CourseInc dx dy) = ct (CourseStep 0 0 0)
  where
    ct (CourseStep x y tc) = case coursePos c x y of
      Nothing -> tc
      (Just res) -> ct (CourseStep (x + dx) (y + dy) (if res == Tree then tc + 1 else tc))

day3_1 = (flip countTrees (CourseInc 3 1)) <$> getCourse

day3_2 =
  ( \course ->
      getProduct $
        foldMap' Product $
          countTrees course
            <$> [CourseInc 1 1, CourseInc 3 1, CourseInc 5 1, CourseInc 7 1, CourseInc 1 2]
  )
    <$> getCourse
