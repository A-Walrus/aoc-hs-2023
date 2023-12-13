module Solutions.Day11 (solution, part1, part2, parse) where

import Base
import Data.List

solution :: String -> IO ()
solution = run (Day parse part1 part2)

type Parsed = [[Bool]]

parse :: String -> Parsed
parse = (map . map) (== '#') . lines

part1 :: Parsed -> Int
part1 = solve 2

solve :: Int -> Parsed -> Int
solve expansion image = sum $ map (uncurry dist) $ pairs galaxies
  where
    numGalaxies = length $ filter id (concat image)
    emptyRows = findIndices (not . or) image
    emptyCols = findIndices (not . or) (transpose image)

    cells = concatMap (\(l, y) -> zipWith (\c x -> ((x, y), c)) l [0 ..]) (zip image [0 ..])

    galaxies = map (newPos . fst) $ filter snd cells

    newPos :: Pos -> Pos
    newPos (x, y) = (x + expanded x emptyCols, y + expanded y emptyRows)

    expanded a l = (expansion - 1) * length (takeWhile (< a) l)

    pairs :: [a] -> [(a, a)]
    pairs l = [(x, y) | (x : ys) <- tails l, y <- ys]

    dist :: Pos -> Pos -> Int
    dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

part2 :: Parsed -> Int
part2 = solve 1000000
