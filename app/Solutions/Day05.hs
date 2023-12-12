module Solutions.Day05 (solution, part1, part2, parse) where

import Base
import Data.List
import Data.List.Split (chunksOf, splitWhen)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)

solution :: String -> IO ()
solution = run (Day parse part1 part2)

type Parsed = ([Int], [[Mapping]])

data Mapping = Mapping {destStart :: Int, sourceStart :: Int, rangeLen :: Int} deriving (Show)

parse :: String -> Parsed
parse s = (seeds, maps)
  where
    (first : _ : rest) = lines s
    seeds = map read $ tail $ words first
    maps =
      map
        (map parseMapping . filter (/= ""))
        (tail $ splitWhen (elem ':') rest)
    parseMapping :: String -> Mapping
    parseMapping = (\[a, b, c] -> Mapping a b c) . map read . words

part1 :: Parsed -> Int
part1 (seeds, maps) = minimum $ map f seeds
  where
    f :: Int -> Int
    f seed = foldl process seed maps

    process :: Int -> [Mapping] -> Int
    process val mapping = case mapMaybe (\(Mapping dest start len) -> if (val - start < len) && (val - start) >= 0 then Just (dest + val - start) else Nothing) mapping of
      [res] -> res
      _ -> val

part2 :: Parsed -> Int
part2 (s, maps) = minimum $ map fst f 
  where
    seeds = map (\[a, b] -> (a, b)) $ chunksOf 2 s
    f = foldl (\a b -> concatMap (process b) a) seeds maps

process :: [Mapping] -> (Int, Int) -> [(Int, Int)]
process mappings range = foldr (concatMap . process1) [range] mappings

process1 :: Mapping -> (Int, Int) -> [(Int, Int)]
process1 (Mapping dest mStart mLen) (rStart, rLen) = if oLen > 0 then overlap : before ++ after else [(rStart, rLen)]
  where
    mEnd = mStart + mLen
    rEnd = rStart + rLen
    (oStart, oEnd) = (max rStart mStart, min rEnd mEnd)
    oLen = oEnd - oStart
    overlap = (oStart + dest - mStart, oLen)
    before = [(rStart, mStart - rStart) | rStart < mStart && mStart < rEnd]
    after = [(mEnd, rEnd - mEnd) | mEnd < rEnd && mEnd > rStart]
