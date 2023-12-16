module Solutions.Day16 (solution, part1, part2, parse) where

import Base
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

solution :: String -> IO ()
solution = run (Day parse part1 part2)

type Parsed = (Map.Map Pos Char, Pos)

parse :: String -> Parsed
parse s = (m, dims)
  where
    ls = lines s
    m = Map.fromList (concat $ zipWith (\l y -> zipWith (\c x -> ((x, y), c)) l [0 ..]) ls [0 ..])
    dims = (length (head ls), length ls)

type State = Set.Set (Pos, Dir)

part1 :: Parsed -> Int
part1 (m, _) = solve (0, 0) East m

part2 :: Parsed -> Int
part2 (grid, (width, height)) = maximum $ map (\(pos, dir) -> solve pos dir grid) $ concat [top, bottom, left, right]
  where
    top = [((x, 0), South) | x <- [0 .. width - 1]]
    bottom = [((x, height - 1), North) | x <- [0 .. width - 1]]
    left = [((0, y), East) | y <- [0 .. height - 1]]
    right = [((width - 1, y), West) | y <- [0 .. height - 1]]

solve :: Pos -> Dir -> Map.Map Pos Char -> Int
solve start dir grid = length energized
  where
    initial = Set.fromList [(start, d) | d <- collide dir (grid Map.! start)]

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
