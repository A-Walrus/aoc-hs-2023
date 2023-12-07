module Solutions.Day02 (solution, part1, part2, parse) where

import Base
import Data.List
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)

solution :: String -> IO ()
solution = run (Day parse part1 part2)

data Game = Game Int Int Int deriving (Show)

type Parsed = [[Game]]

parse :: String -> Parsed
parse = map parseLine . lines
  where
    mainPart = last . splitOn ":"
    parseGame :: String -> Game
    parseGame s = Game red green blue
      where
        splits = splitOn "," s
        getColor color = maybe 0 (read. head . words) (find (color `isSuffixOf`) splits)
        red = getColor "red"
        green = getColor "green"
        blue = getColor "blue"
    parseLine = map parseGame . splitOn ";" . mainPart

part1 :: Parsed -> Int
part1 = sum . map (+ 1) . findIndices (all possible)
  where
    possible (Game red green blue) = red <= 12 && green <= 13 && blue <= 14

part2 :: Parsed -> Int
part2 = sum . map power
  where
    power :: [Game]->Int
    power = multiply . foldr union (Game 0 0 0)
    multiply (Game r g b) = r * g * b
    union (Game r1 g1 b1) (Game r2 g2 b2) = Game (max r1 r2) (max g1 g2) (max b1 b2)
