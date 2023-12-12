module Solutions.Day10 (solution, part1, part2, parse) where

import Base
import Data.List
import qualified Data.Map as Map
import Data.Maybe (fromJust, isNothing, mapMaybe)

solution :: String -> IO ()
solution = run (Day parse part1 part2)

type Parsed = (Pos, Map.Map Pos Pipe)

type Pipe = [Dir]

parse :: String -> Parsed
parse s = (start, newPipes)
  where
    ls = zip (lines s) [0 ..]
    cells = concatMap (\(l, y) -> zipWith (\c x -> ((x, y), c)) l [0 ..]) ls
    pipes = Map.fromList $ mapMaybe (\(p, c) -> if isPipe c then Just (p, charToPipe c) else Nothing) cells
    start = (\(l, y) -> (fromJust $ elemIndex 'S' l, y)) $ fromJust $ find (\(l, y) -> 'S' `elem` l) ls
    isPipe = (`elem` "J|LF-7")
    charToPipe 'J' = [West, North]
    charToPipe '|' = [South, North]
    charToPipe 'L' = [North, East]
    charToPipe 'F' = [East, South]
    charToPipe '-' = [East, West]
    charToPipe '7' = [South, West]

    aroundStart = filter f [North, East, South, West]
    f dir = newPos `Map.member` pipes && opposite dir `elem` (pipes Map.! newPos)
      where
        newPos = add start (vec dir)

    newPipes = Map.insert start aroundStart pipes

part1 :: Parsed -> Int
part1 (start, pipes) = walk `div` 2
  where
    walk = length $ path start pipes

path start pipes = takeWhile (\(pos, prev) -> pos /= start || prev == start) $ iterate step (start, start)
  where
    step (pos, prev) = (new, pos)
      where
        pipe = pipes Map.! pos
        new = fromJust $ find (/= prev) $ map (add pos . vec) pipe

part2 :: Parsed -> Int
part2 (start, ps) = length $ filter (`inside` North) spots
  where
    positions = Map.keys ps
    topLeft :: Pos
    bottomRight :: Pos
    topLeft = foldr (\(x1, y1) (x2, y2) -> (min x1 x2, min y1 y2)) bottomRight loop
    bottomRight = foldr (\(x1, y1) (x2, y2) -> (max x1 x2, max y1 y2)) (0,0) loop

    spots = filter (`notElem` loop) $ [(a, b) | a <- [x topLeft .. x bottomRight], b <- [y topLeft.. y bottomRight]]

    loop = map fst $ path start ps

    pipes = Map.filterWithKey (\k _ -> k `elem` loop) ps

    inside pos dir = fst $ foldr f (False, Nothing) $ takeWhile inBounds $ map (add pos . scale (vec dir)) [1 ..]
      where
        f p acc@(a, b)
          | not (p `Map.member` pipes) = acc
          | length inter == 2 = (not a, Nothing)
          | inter /= [] =
              if isNothing b
                then (a, Just d)
                else if Just d == b then (a, Nothing) else (not a, Nothing)
          | otherwise = acc
          where
            inter = orthogonal dir `intersect` (pipes Map.! p)
            [d] = inter

    orthogonal North = [East, West]
    orthogonal East = [South, North]
    orthogonal x = orthogonal (opposite x)
    inBounds (a, b) = x topLeft <= a && a <= x bottomRight && y topLeft <= b && b <= y bottomRight
