module Solutions.Day14 (solution, part1, part2, parse) where

import Base
import Data.List
import Data.List.NonEmpty (groupWith, toList)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import qualified Data.Set as Set

solution :: String -> IO ()
solution = run (Day parse part1 part2)

type Parsed = [[Char]]

parse :: String -> Parsed
parse = lines

part1 :: Parsed -> Int
part1 = load . tilt North

part2 :: Parsed -> Int
part2 grid = load (loop !! pos)
  where
    tilts = map tilt [North, West, South, East]
    cycle a = foldl (\a f -> f a) a tilts
    tillRepeat = map fst $ takeWhile (\(a, b) -> not (a `Set.member` b)) $ iterate (\(state, prevs) -> (cycle state, Set.insert state prevs)) (grid, Set.empty)
    first = fromJust $ elemIndex (cycle (last tillRepeat)) tillRepeat
    loop = drop first tillRepeat
    pos = (1000000000 - first) `mod` length loop

tilt :: Dir -> [[Char]] -> [[Char]]
tilt North grid = transpose $ tilt West (transpose grid)
tilt West grid = tilted
  where
    sections = map (map toList . groupWith (== '#')) grid
    tilted = map (concatMap (sortBy (comparing rank))) sections
    rank '#' = -1 -- Doesn't matter what value is here
    rank 'O' = 0 -- `O` is smaller than `.` so that it appears first when sorted
    rank '.' = 1
tilt East grid = map reverse $ tilt West (map reverse grid)
tilt South grid = reverse $ tilt North (reverse grid)

load l = sum . zipWith (*) [numRows, numRows - 1 ..] $ map (length . filter (== 'O')) l
  where
    numRows = length l
