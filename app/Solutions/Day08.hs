module Solutions.Day08 (solution, part1, part2, parse) where

import Base
import Data.List
import qualified Data.Map as Map

solution :: String -> IO ()
solution = run (Day parse part1 part2)

type Parsed = ([Dir], Map.Map Node (Node, Node))

type Node = String

data Dir = L | R deriving (Show)

parse :: String -> Parsed
parse s = (map parseDir f, Map.fromList $ map parseLine xs)
  where
    (f : _ : xs) = lines s
    kaka = map parseLine xs
    parseLine = (\[a, _, l, r] -> (a, ((init . tail) l, init r))) . words
    parseDir 'L' = L
    parseDir 'R' = R

part1 :: Parsed -> Int
part1 (dirs, map) = 1 + snd (last $ takeWhile ((/= "ZZZ") . fst) $ scanl (\(pos, count) dir -> (getByDir pos dir, count + 1)) ("AAA", 0) (cycle dirs))
  where
    getByDir pos L = fst $ map Map.! pos
    getByDir pos R = snd $ map Map.! pos

part2 :: Parsed -> Int
part2 = undefined
