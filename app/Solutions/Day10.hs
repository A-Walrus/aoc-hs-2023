module Solutions.Day10 (solution, part1, part2, parse) where

import Base
import Data.List
import qualified Data.Map as Map
import Data.Maybe (fromJust, mapMaybe)

solution :: String -> IO ()
solution = run (Day parse part1 part2)

type Parsed = (Pos, Map.Map Pos Pipe)

type Pipe = [Dir]

parse :: String -> Parsed
parse s = (start, Map.fromList pipes)
  where
    ls = zip (lines s) [0 ..]
    cells = concatMap (\(l, y) -> zipWith (\c x -> ((x, y), c)) l [0 ..]) ls
    pipes = mapMaybe (\(p, c) -> if isPipe c then Just (p, charToPipe c) else Nothing) cells
    start = (\(l, y) -> (fromJust $ elemIndex 'S' l, y)) $ fromJust $ find (\(l, y) -> 'S' `elem` l) ls
    isPipe = (`elem` "J|LF-7")
    charToPipe 'J' = [West, North]
    charToPipe '|' = [South, North]
    charToPipe 'L' = [North, East]
    charToPipe 'F' = [East, South]
    charToPipe '-' = [East, West]
    charToPipe '7' = [South, West]

part1 :: Parsed -> Int
part1 (start, p) = walk `div` 2
  where
    aroundStart = filter f [North, East, South, West]
    f dir = newPos `Map.member` pipes && opposite dir `elem` (pipes Map.! newPos)
      where
        newPos = add start (vec dir)

    pipes = Map.insert start aroundStart p

    walk = length $ takeWhile (\(pos, prev) -> pos /= start || prev == start) $ iterate step (start, start)

    step (pos, prev) = (new, pos)
      where
        pipe = pipes Map.! pos
        new = fromJust $ find (/= prev) $ map (add pos . vec) pipe

part2 :: Parsed -> Int
part2 = undefined
