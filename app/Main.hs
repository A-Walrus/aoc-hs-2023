{-# LANGUAGE NamedFieldPuns #-}

import Base
import Data.Maybe
import qualified Solutions.Day01 as Day01
import qualified Solutions.Day02 as Day02
import qualified Solutions.Day03 as Day03
import qualified Solutions.Day04 as Day04
import qualified Solutions.Day05 as Day05
import qualified Solutions.Day06 as Day06
import qualified Solutions.Day07 as Day07
import qualified Solutions.Day08 as Day08
import qualified Solutions.Day09 as Day09
import qualified Solutions.Day10 as Day10
import qualified Solutions.Day11 as Day11
import qualified Solutions.Day12 as Day12
import qualified Solutions.Day13 as Day13
import System.Environment
import Text.Printf

days :: [String -> IO ()]
days =
  [ Day01.solution,
    Day02.solution,
    Day03.solution,
    Day04.solution,
    Day05.solution,
    Day06.solution,
    Day07.solution,
    Day08.solution,
    Day09.solution,
    Day10.solution,
    Day11.solution,
    Day12.solution,
    Day13.solution
  ]

data Args
  = RunDay
      { day :: Int,
        path :: Maybe String
      }
  | All

parseArgs :: [String] -> Args
parseArgs [] = All
parseArgs ["all"] = All
parseArgs [day] = RunDay {day = read day, path = Nothing}
parseArgs [day, path] = RunDay {day = read day, path = Just path}
parseArgs _ = error "Not enough args"

main :: IO ()
main = do
  a <- getArgs
  let args = parseArgs a
  runArgs args

runArgs :: Args -> IO ()
runArgs RunDay {day, path} = runDay day path
runArgs All = mapM_ (`runDay` Nothing) [1..length days]

runDay :: Int -> Maybe String -> IO ()
runDay day path = do
  printf " -- AOC Day %d -- \n" day
  let path' = fromMaybe "input" path
  let solution = days !! (day - 1)
  let filePath = printf "inputs/%02d/%s" day path'
  contents <- readFile filePath
  solution contents
  putStrLn ""
