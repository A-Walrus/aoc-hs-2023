module Solutions.DayXX (solution) where

import Base
import Data.List

solution :: String -> IO ()
solution = run (Day parse part1 part2)

type Parsed = ()

parse :: String -> Parsed
parse = undefined

part1 :: Parsed -> Int
part1 = undefined

part2 :: Parsed -> Int
part2 = undefined