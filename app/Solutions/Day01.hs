module Solutions.Day01 (solution, part1, part2) where

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
part2 = sum . map (\s -> (10 * firstDigit s) + lastDigit (reverse s))
  where
    firstDigit s@(x : xs)
      | isDigit x = digitToInt x
      | otherwise = case getPrefix s digits of
          Just x -> x
          _ -> firstDigit xs

    lastDigit s@(x : xs)
      | isDigit x = digitToInt x
      | otherwise = case getPrefix s (map reverse digits) of
          Just x -> x
          _ -> lastDigit xs

    getPrefix s = fmap (+ 1) . findIndex (`isPrefixOf` s)

    digits =
      [ "one",
        "two",
        "three",
        "four",
        "five",
        "six",
        "seven",
        "eight",
        "nine"
      ]
