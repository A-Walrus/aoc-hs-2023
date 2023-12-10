module Solutions.Day06 (solution, part1, part2, parse) where

import Base
import Data.List

solution :: String -> IO ()
solution = run (Day parse part1 part2)

parse = id

part1 :: String -> Int
part1 = product . map margin . parse
  where
    parse :: String -> [(Int, Int)]
    parse = map tuplify . transpose . map (map read . tail . words) . lines


margin :: (Int, Int) -> Int
margin r = lastValid r - firstValid r + exactComp r
  where
    det (t, d) = sqrt (fromIntegral $ (t * t) - 4 * d)
    firstValid (t, d) = ceiling $ (fromIntegral t - det (t, d)) / 2
    lastValid (t, d) = floor $ (fromIntegral t + det (t, d)) / 2
    exactComp (t, d) = if floor (det (t, d)) == ceiling (det (t, d)) then -1 else 1

part2 :: String -> Int
part2 = margin . parse
  where
    parse :: String -> (Int, Int)
    parse = tuplify . map (read . concat . tail . words) . lines
