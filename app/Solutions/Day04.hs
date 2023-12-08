module Solutions.Day04 (solution, part1, part2, parse) where

import Base
import Data.List
import Data.List.Split (splitOn)

solution :: String -> IO ()
solution = run (Day parse part1 part2)

type Parsed = [([Int], [Int])]

parse :: String -> Parsed
parse = map parseLine . lines
  where
    parseLine = (\[a, b] -> (a, b)) . map (map read . words) . splitOn "|" . last . splitOn ":"

part1 :: Parsed -> Int
part1 = sum . map score
  where
    score lists = if count lists == 0 then 0 else 2 ^ (count lists - 1)

count :: ([Int], [Int]) -> Int
count (a, b) = length (filter (`elem` a) b)

part2 :: Parsed -> Int
part2 cards = snd $ foldl (\(x : xs, sum) card -> (zipWith (+) xs (replicate (count card) x ++ repeat 0), sum + x)) (counts, 0) cards
  where
    counts = map (const 1) cards
