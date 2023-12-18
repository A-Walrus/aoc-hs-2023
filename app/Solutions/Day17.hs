module Solutions.Day17 (solution, part1, part2, parse) where

import Base
import Data.Char (digitToInt)
import Data.List
import qualified Data.Map as Map

solution :: String -> IO ()
solution = run (Day parse part1 part2)

type Parsed = [[Int]]

parse :: String -> Parsed
parse = (map . map) digitToInt . lines

type WalkState = (Dir, Int)

type State = Map.Map (Pos, WalkState) Int

type Entry = ((Pos, WalkState), Int)

part1 :: Parsed -> Int
part1 = solve check 0
  where
    check c dir d = d /= opposite dir && (d /= dir || c < 3)

part2 :: Parsed -> Int
part2 = solve check 4
  where
    -- check c dir d = d /= opposite dir && (d /= dir || c < 10)
    check c dir d =
      d /= opposite dir
        && if c < 4
          then d == dir
          else c < 10 || d /= dir

solve :: (Int -> Dir -> Dir -> Bool) -> Int -> Parsed -> Int
solve check minCount grid = minimum atEnd
  where
    width = length (head grid)
    height = length grid
    end = (width - 1, height - 1)

    inBounds :: Pos -> Bool
    inBounds (x, y) = x >= 0 && y >= 0 && x < width && y < height

    initial = Map.fromList [(((0, 0), (South, 0)), 0)] -- could be any `Dir`
    i = iterate f (initial, Map.empty)
    final = uncurry Map.union $ last $ takeWhile (not . Map.null . fst) i
    atEnd = map snd $ filter (\((p, (_, c)), _) -> p == end && c >= minCount) $ Map.assocs final

    f :: (State, State) -> (State, State)
    f (new, old) = (new', old')
      where
        x = Map.fromListWith min $ concatMap step $ Map.assocs new
        old' = Map.unionWith min new old
        new' = Map.differenceWith (\a b -> if a < b then Just a else Nothing) x old'

    step :: Entry -> [Entry]
    step ((pos, (dir, count)), heat) = map f possibleDirs
      where
        possibleDirs = filter (inBounds . add pos . vec) $ filter ((count == 0 ||) . check count dir) [North, West, South, East]

        f d = ((newPos, (d, newCount)), newHeat)
          where
            newPos = add (vec d) pos
            newHeat = heat + (grid !! y newPos !! x newPos)
            newCount = if d == dir then count + 1 else 1
