{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module Base where

import Control.Exception (SomeException, try)
import Data.Bifunctor
import Debug.Trace
import GHC.IO (catch)

debug :: (Show a) => String -> a -> a
debug s v = trace (s ++ ": " ++ show v) v

dMap f = bimap f f

type Pos = (Int, Int)

add :: Pos -> Pos -> Pos
add a = uncurry bimap (dMap (+) a)

scale :: Pos -> Int -> Pos
scale (x, y) n = (x * n, y * n)

y :: Pos -> Int
y = snd

x :: Pos -> Int
x = fst

data Dir = North | East | South | West deriving (Eq, Show, Ord, Enum)

opposite :: Dir -> Dir
opposite North = South
opposite East = West
opposite South = North
opposite West = East

vec :: Dir -> Pos
vec North = (0, -1)
vec East = (1, 0)
vec South = (0, 1)
vec West = (-1, 0)

tuplify :: [a] -> (a, a)
tuplify [a, b] = (a, b)

data Day a b c = Day {parse' :: String -> a, part1' :: a -> b, part2' :: a -> c}

run :: (Print b, Print c) => Day a b c -> String -> IO ()
run Day {parse', part1', part2'} s = do
  let parsed = parse' s
  putStr "Part 1: "
  catch (putStrLn $ string (part1' parsed)) f
  putStr "Part 2: "
  catch (putStrLn $ string (part2' parsed)) f
  where
    f :: SomeException -> IO ()
    f e = do
      putStr "Error - "
      print e
      return ()

dummySolution :: String -> IO ()
dummySolution = const (putStrLn "Dummy Solution")

class Print a where
  string :: a -> String

instance Print String where
  string = id

instance Print Int where
  string = show

instance Print Integer where
  string = show
