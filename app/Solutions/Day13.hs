module Solutions.Day13 (solution, part1, part2, parse) where

import Base
import Data.List
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

solution :: String -> IO ()
solution = run (Day parse part1 part2)

type Parsed = [Area]

type Area = [String]

parse :: String -> Parsed
parse = map lines . splitOn "\n\n"

part1 :: Parsed -> Int
part1 = sum . map (mirrorVal mirrored)
  where
    mirrored a b = and $ zipWith (==) (reverse a) b

part2 :: Parsed -> Int
part2 = sum . map (mirrorVal almostMirrored)
  where
    almostMirrored a b = (== 1) $ length $ filter not $ concat $ zipWith (zipWith (==)) (reverse a) b

mirrorLine f area = (+ 1) <$> findIndex (uncurry f) [splitAt p area | p <- [1 .. length area - 1]]

mirrorVal f area = case mirrorLine f area of
  Just a -> 100 * a
  Nothing -> fromJust (mirrorLine f (transpose area))
