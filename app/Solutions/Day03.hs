module Solutions.Day03 (solution, part1, part2, parse) where

import Base
import Data.Char (isDigit)
import Data.List
import Data.List.NonEmpty (groupWith)

solution :: String -> IO ()
solution = run (Day parse part1 part2)

type Parsed = [String]

parse :: String -> Parsed
parse = lines

part1 :: Parsed -> Int
part1 s = sum $ zipWith (\n l -> sum $ map snd $ filter (\(pos, _) -> nextToSymbol l pos) n) numbers [0 ..]
  where
    width = length $ head s
    padded = replicate width '.' : s ++ [replicate width '.']
    symbols :: [[Int]]
    symbols = map (findIndices (\x -> not (isDigit x) && x /= '.')) padded

    numbers :: [[((Int, Int), Int)]]
    numbers = map (map summarize . filter (dig . head) . groupBy (\a b -> dig a && dig b) . zip [0 ..]) padded
      where
        dig = isDigit . snd
        summarize l = ((fst (head l), fst (last l)), read $ map snd l)
        consecutive (a, _) (b, _) = a + 1 == b

    nextToSymbol :: Int -> (Int, Int) -> Bool
    nextToSymbol line (start, end) = onLine line || onLine (line + 1) || onLine (line - 1)
      where
        onLine l = any (\x -> x >= start - 1 && x <= end + 1) $ symbols !! l

part2 :: Parsed -> Int
part2 = undefined
