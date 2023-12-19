module Solutions.Day18 (solution, part1, part2, parse) where

import Base
import Data.List
import qualified Data.Set as Set

solution :: String -> IO ()
solution = run (Day parse part1 part2)

type Parsed = [(Dir, Int, Color)]

type Color = String

parse :: String -> Parsed
parse = map ((\[dir, count, color] -> (readDir dir, read count, readColor color)) . words) . lines
  where
    readDir "L" = West
    readDir "R" = East
    readDir "D" = South
    readDir "U" = North
    readColor = take 6 . drop 2

part1 :: Parsed -> Int
part1 p = solve plan
  where
    plan = map (\(a,b,_) -> (a,b)) p



solve :: [(Dir,Int)] -> Int
solve plan = length perimeter + length inside
  where
    origin = (0, 0) -- arbitrary
    perimeter = Set.fromList $ scanl (\pos dir -> add pos (vec dir)) origin moves
    moves = concatMap (\(dir, count) -> replicate count dir) plan
    bot_right = Set.fold (\(x1, y1) (x2, y2) -> (max x1 x2, max y1 y2)) origin perimeter
    top_left = Set.fold (\(x1, y1) (x2, y2) -> (min x1 x2, min y1 y2)) origin perimeter

    start :: Pos
    start = (1, 1) -- HACK this only works for this specific input.

    inside = uncurry Set.union $ last $ takeWhile (not . Set.null . fst) $ iterate f (Set.singleton start, Set.empty)

    f (new, old) = (new', old')
      where
        s = Set.fromList $ concatMap spread $ Set.toList new
        new' = Set.difference s old'
        old' = Set.union new old

    spread pos = filter (`Set.notMember` perimeter) $ map (add pos . vec) [North, South, East, West]


part2 :: Parsed -> Int
part2 p = solve plan
  where
    plan = map (\(_, _, x) -> parseColor x) p

parseColor :: String -> (Dir, Int)
parseColor color = (dir last, read ("0x" ++ dist))
  where
    (dist, last) = splitAt 5 color
    dir "0" = East
    dir "1" = South
    dir "2" = West
    dir "3" = North
