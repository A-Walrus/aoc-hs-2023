module Solutions.Day09 (solution, part1, part2, parse) where

import Base
import Data.List

solution :: String -> IO ()
solution = run (Day parse part1 part2)

type Parsed = [[Int]]

parse :: String -> Parsed
parse = map (map read . words) . lines

part1 :: Parsed -> Int
part1 = sum . map nextVal
  where
    nextVal l
      | all (==0) l = 0
      | otherwise = last l + nextVal (diffs l)
    diffs l = zipWith (-) (tail l) l

part2 :: Parsed -> Int
part2 = undefined
