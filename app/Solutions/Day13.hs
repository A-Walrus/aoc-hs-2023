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
part1 = sum . map mirrorVal
  where
    mirrorLine area = (+1) <$> findIndex (uncurry mirrored) [splitAt p area | p <- [1 .. length area - 1]]

    mirrored a b = and $ zipWith (==) (reverse a) b

    mirrorVal area = case mirrorLine area of
      Just a -> 100 * a
      Nothing -> fromJust (mirrorLine (transpose area))

part2 :: Parsed -> Int
part2 = undefined
