module Solutions.Day05 (solution, part1, part2, parse) where

import Base
import Data.List
import Data.List.Split (splitWhen)
import Data.Maybe (mapMaybe)

solution :: String -> IO ()
solution = run (Day parse part1 part2)

type Parsed = ([Int], [[Mapping]])

data Mapping = Mapping {destStart :: Int, sourceStart :: Int, rangeLen :: Int} deriving (Show)

parse :: String -> Parsed
parse s = (seeds, maps)
  where
    (first : _ : rest) = lines s
    seeds = map read $ tail $ words first
    maps =
      map
        (map parseMapping . filter (/= ""))
        (tail $ splitWhen (elem ':') rest)
    parseMapping :: String -> Mapping
    parseMapping = (\[a, b, c] -> Mapping a b c) . map read . words

part1 :: Parsed -> Int
part1 (seeds, maps) = minimum $ map f seeds
  where
    f :: Int -> Int
    f seed = foldl process seed maps

process :: Int -> [Mapping] -> Int
process val mapping = case mapMaybe (\(Mapping dest start len) -> if (val - start < len) && (val - start) >= 0 then Just (dest + val - start) else Nothing) mapping of
  [res] -> res
  _ -> val

part2 :: Parsed -> Int
part2 = undefined
