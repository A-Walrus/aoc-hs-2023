module Solutions.Day12 (solution, part1, part2, parse) where

import Base
import Data.List
import Data.List.Split (splitOn)

solution :: String -> IO ()
solution = run (Day parse part1 part2)

type Parsed = [([State], [Int])]

data State = Black | White | Unknown deriving (Show, Eq)

parse :: String -> Parsed
parse = map f . lines
  where
    f = (\[tiles, counts] -> (map parseTile tiles, map read $ splitOn "," counts)) . words
    parseTile '.' = White
    parseTile '#' = Black
    parseTile '?' = Unknown


-- Attempted 6902, it was too low.
part1 :: Parsed -> Int
part1 = sum . map (uncurry numOptions)
  where
    numOptions tiles counts = length $ filter (matches tiles) (allOptions counts (length tiles))
    matches a b = and $ zipWith (\x y -> x == y || y == Unknown || x == Unknown) a b

    allOptions [] size = [replicate size White]
    allOptions [x] size = [replicate gap White ++ replicate x Black ++ replicate (size - gap - x) White | gap <- [0 .. (size - x)]]
    allOptions l@(x : xs) size = [replicate gap White ++ replicate x Black ++ [White] ++ rest | gap <- [0 .. maxGap], rest <- allOptions xs (size - gap - x - 1)]
      where
        maxGap = sum l + length l - 1

part2 :: Parsed -> Int
part2 = undefined
