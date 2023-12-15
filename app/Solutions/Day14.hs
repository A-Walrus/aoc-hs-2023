module Solutions.Day14 (solution, part1, part2, parse) where

import Base
import Data.List
import Data.List.NonEmpty (groupWith, toList)
import Data.Ord (comparing)

solution :: String -> IO ()
solution = run (Day parse part1 part2)

type Parsed = [[Char]]

parse :: String -> Parsed
parse = lines

part1 :: Parsed -> Int
part1 rows = score tilted
  where
    cols = transpose rows
    sections = map (map toList . groupWith (== '#')) cols
    tilted = transpose $ map (concatMap (sortBy (comparing rank))) sections
    rank '#' = -1 -- Doesn't matter what value is here
    rank 'O' = 0 -- `O` is smaller than `.` so that it appears first when sorted
    rank '.' = 1

    numRows = length rows
    score l = sum . zipWith (*) [numRows, numRows - 1 ..] $ map (length . filter (== 'O')) l

part2 :: Parsed -> Int
part2 = undefined
