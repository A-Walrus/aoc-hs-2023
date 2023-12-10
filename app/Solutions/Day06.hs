module Solutions.Day06 (solution, part1, part2, parse) where

import Base
import Data.List

solution :: String -> IO ()
solution = run (Day parse part1 part2)

type Parsed = [(Int, Int)]

parse :: String -> Parsed
parse = map (\[a, b] -> (a, b)) . transpose . map (map read . tail . words) . lines

part1 :: Parsed -> Int
part1 = product . map (\r -> lastValid r - firstValid r + exactComp r)
  where
    det (t, d) =  sqrt (fromIntegral $ (t * t) - 4 * d)
    firstValid (t,d) = ceiling $ (fromIntegral t - det (t,d)) / 2
    lastValid (t,d) = floor $ (fromIntegral t + det (t,d)) / 2
    exactComp (t,d)= if floor (det (t,d)) == ceiling (det(t,d)) then -1 else 1

part2 :: Parsed -> Int
part2 = undefined
