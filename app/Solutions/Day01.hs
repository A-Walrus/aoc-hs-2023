module Solutions.Day01 (solution) where

import Base
import Data.Char (digitToInt, isDigit)
import Data.List

solution :: String -> IO ()
solution = run (Day parse part1 part2)

type Parsed = [String]

parse :: String -> Parsed
parse = lines

part1 :: Parsed -> Int
part1 = sum . map f
  where
    f = (\l -> (10 * digitToInt (head l)) + digitToInt (last l)) . filter isDigit

part2 :: Parsed -> Int
part2 = undefined
