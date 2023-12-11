module Solutions.Day08 (solution, part1, part2, parse) where

import Base
import Data.List
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust)

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
part1  = path "AAA" (=="ZZZ")


path start endCondition (dirs,maps)= 1 + snd (last $ takeWhile (not . endCondition . fst) $ scanl (\(pos, count) dir -> (getByDir pos dir, count + 1)) (start, 0) (cycle dirs))
  where
    getByDir pos L = fst $ maps Map.! pos
    getByDir pos R = snd $ maps Map.! pos

part2 :: Parsed -> Int
part2 p@(dirs, maps) = foldr leastCommonMultiple 1 toZ
  where
    lastIs c = (== c) . last
    starts = filter (lastIs 'A') $ Map.keys maps
    toZ = map (\s -> path s (lastIs 'Z') p) starts

    leastCommonMultiple :: Int -> Int -> Int
    leastCommonMultiple a b = (a * b) `div` greatestCommonDivisor a b

    greatestCommonDivisor :: Int -> Int -> Int
    greatestCommonDivisor a b = euclid (max a b) (min a b)
      where
        euclid a 0 = a
        euclid a b = euclid b (a `mod` b)
