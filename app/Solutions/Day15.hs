module Solutions.Day15 (solution, part1, part2, parse) where

import Base
import Data.List
import Data.List.Split (splitOn)
import Data.Char (ord)

solution :: String -> IO ()
solution = run (Day parse part1 part2)

type Parsed = [String]

parse :: String -> Parsed
parse = splitOn "," . init

part1 :: Parsed -> Int
part1 = sum .  map hash

hash :: String -> Int
hash = foldl (\val c -> (17 * ( ord c + val)) `mod` 256 ) 0

part2 :: Parsed -> Int
part2 = undefined
