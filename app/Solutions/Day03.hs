{-# LANGUAGE TupleSections #-}

module Solutions.Day03 (solution, part1, part2, parse) where

import Base
import Data.Char (isDigit)
import Data.List
import Data.List.NonEmpty (groupWith)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

solution :: String -> IO ()
solution = run (Day parse part1 part2)

type Parsed = [String]

parse :: String -> Parsed
parse = lines

part1 :: Parsed -> Int
part1 s = sum $ zipWith (\n l -> sum $ map snd $ filter (\(pos, _) -> nextToSymbol l pos (symbols padded)) n) (numbers padded) [0 ..]
  where
    width = length $ head s
    padded = replicate width '.' : s ++ [replicate width '.']

nextToSymbol :: Int -> (Int, Int) -> [[Int]] -> Bool
nextToSymbol line (start, end) symbols = onLine line || onLine (line + 1) || onLine (line - 1)
  where
    onLine l = any (\x -> x >= start - 1 && x <= end + 1) $ symbols !! l

part2 :: Parsed -> Int
part2 s = sum $ map ratio gears
  where
    width = length $ head s
    padded = replicate width '.' : s ++ [replicate width '.']
    gears = concat $ zipWith (\list line -> map (,line) list) (map (elemIndices '*') padded) [0 ..]
    ratio pos = if length around == 2 then product around else 0
      where
        around = numbersAround pos

    nums = numbers padded
    numbersAround :: (Int, Int) -> [Int]
    numbersAround (x, y) = onLine y ++ onLine (y - 1) ++ onLine (y + 1)
      where
        onLine l = mapMaybe (\((start, end), n) -> if start - 1 <= x && x <= end + 1 then Just n else Nothing) $ nums !! l

symbols :: [String] -> [[Int]]
symbols = map (findIndices (\x -> not (isDigit x) && x /= '.'))

numbers :: [String] -> [[((Int, Int), Int)]]
numbers = map (map summarize . filter (dig . head) . groupBy (\a b -> dig a && dig b) . zip [0 ..])
  where
    dig = isDigit . snd
    summarize l = ((fst (head l), fst (last l)), read $ map snd l)
    consecutive (a, _) (b, _) = a + 1 == b
