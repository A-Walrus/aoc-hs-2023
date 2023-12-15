module Solutions.Day15 (solution, part1, part2, parse) where

import Base
import Data.Char (ord)
import Data.List
import Data.List.Split (splitOn)
import qualified Data.Map as Map

solution :: String -> IO ()
solution = run (Day parse part1 part2)

type Parsed = [String]

data Command = Insert String Int | Remove String deriving (Show)

parse :: String -> Parsed
parse = splitOn "," . init

part1 :: Parsed -> Int
part1 = sum . map hash

hash :: String -> Int
hash = foldl (\val c -> (17 * (ord c + val)) `mod` 256) 0

type Boxes = Map.Map Int [Lens]

type Lens = (String, Int)

part2 :: Parsed -> Int
part2 strings = sum $ map (uncurry focusPowerBox) (Map.toList state)
  where
    state = foldl f (Map.fromList [(i, []) | i <- [0 .. 255]]) commands

    focusPowerBox :: Int -> [Lens] -> Int
    focusPowerBox index = sum . zipWith (\x (_, lens) -> x * lens * (index + 1)) [1 ..]

    f :: Boxes -> Command -> Boxes
    f boxes (Remove label) = Map.adjust (filter ((/= label) . fst)) (hash label) boxes
    f boxes (Insert label lens) = Map.adjust (insert (label, lens)) (hash label) boxes

    insert :: Lens -> [Lens] -> [Lens]
    insert (label, lens) list =
      if any ((== label) . fst) list
        then map (\x@(l, _) -> if l == label then (label, lens) else x) list
        else list ++ [(label, lens)]

    commands = map parseCommand strings
    parseCommand s =
      if last s == '-'
        then Remove (init s)
        else (\[label, lens] -> Insert label (read lens)) $ splitOn "=" s
