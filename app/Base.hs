{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module Base where

import Data.Bifunctor
import Debug.Trace
import Control.Exception (try, SomeException)
import GHC.IO (catch)

debug :: (Show a) => String -> a -> a
debug s v = trace (s ++ ": " ++ show v) v

dMap f = bimap f f

type Pos = (Int, Int)

add :: Pos -> Pos -> Pos
add a = uncurry bimap (dMap (+) a)

y :: Pos -> Int
y = snd

x :: Pos -> Int
x = fst

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
