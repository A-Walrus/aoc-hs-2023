module Solutions.Day16 (solution, part1, part2, parse) where

import Base
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

solution :: String -> IO ()
solution = run (Day parse part1 part2)

type Parsed = Map.Map Pos Char

parse :: String -> Parsed
parse s = Map.fromList (concat $ zipWith (\l y -> zipWith (\c x -> ((x, y), c)) l [0 ..]) (lines s) [0 ..])

type State = Set.Set (Pos, Dir)

part1 :: Parsed -> Int
part1 grid = length energized
  where
    topLeft = (0, 0)
    initial = Set.fromList [(topLeft, dir) | dir <- collide East (grid Map.! topLeft)]

    energized = Set.map fst $ uncurry Set.union $ last run
    run = takeWhile (not . Set.null . fst) $ iterate f (initial, Set.empty)

    f :: (State, State) -> (State, State)
    f (new, old) = (newNew, newOld)
      where
        x = concatMap step new
        newNew = Set.fromList $ filter (`Set.notMember` newOld) x
        newOld = Set.union new old

    step (pos, dir) =
      if newPos `Map.member` grid
        then [(newPos, d) | d <- collide dir (grid Map.! newPos)]
        else []
      where
        newPos = add pos (vec dir)

collide :: Dir -> Char -> [Dir]
collide East '|' = [North, South]
collide West '|' = [North, South]
collide North '|' = [North]
collide South '|' = [South]
collide East '-' = [East]
collide West '-' = [West]
collide North '-' = [East, West]
collide South '-' = [East, West]
collide East '\\' = [South]
collide West '\\' = [North]
collide North '\\' = [West]
collide South '\\' = [East]
collide East '/' = [North]
collide West '/' = [South]
collide North '/' = [East]
collide South '/' = [West]
collide d '.' = [d]

part2 :: Parsed -> Int
part2 = undefined
