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
    plan = map (\(a, b, _) -> (a, b)) p

data Chirality = CW | CCW deriving (Eq)

solve :: [(Dir, Int)] -> Int
solve plan = area plan
  where
    turning = fst $ foldl (\(acc, prev) dir -> (acc + turn prev dir, dir)) (turn (last dirs) (head dirs), head dirs) (tail dirs)
    chirality = case turning of
      4 -> CCW
      -4 -> CW
    dirs = map fst plan

    area :: [(Dir, Int)] -> Int
    area l@[a, b, c, d] = (snd a + 1) * (snd b + 1)
    area l@(a : b : c : d : xs) =
      if fst a == fst c
        then changed + area rest
        else area ((b : c : d : xs) ++ [a])
      where
        rest =
          if snd (combine b d) /= 0
            then combine a c : combine b d : xs
            else combine (combine a c) (head xs) : tail xs
        changed =
          if sign
            then snd b * (snd c + 1)
            else ((-1) * snd b * snd c) + fix
        fix = if fst b /= fst d then snd d else 0
        sign = (turn (fst a) (fst b) == 1) == (chirality == CW)

    combine :: (Dir, Int) -> (Dir, Int) -> (Dir, Int)
    combine a b
      | fst a == fst b = (fst a, snd a + snd b)
      | fst a == opposite (fst b) = if snd a > snd b then (fst a, snd a - snd b) else (fst b, snd b - snd a)
      | otherwise = undefined

    turn North West = 1
    turn West South = 1
    turn South East = 1
    turn East North = 1
    turn a b = -turn a (opposite b)

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
