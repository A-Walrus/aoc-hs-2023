module Solutions.Day07 (solution, part1, part2, parse) where

import Base
import Data.List
import qualified Data.Map as Map
import Data.Ord (comparing)

solution :: String -> IO ()
solution = run (Day parse part1 part2)

type Parsed = [(Hand, Int)]

newtype Hand = Hand [Card] deriving (Eq, Show)

newtype Card = Card Char deriving (Eq, Show)

value :: Card -> Int
value (Card '2') = 2
value (Card '3') = 3
value (Card '4') = 4
value (Card '5') = 5
value (Card '6') = 6
value (Card '7') = 7
value (Card '8') = 8
value (Card '9') = 9
value (Card 'T') = 10
value (Card 'J') = 11
value (Card 'Q') = 12
value (Card 'K') = 13
value (Card 'A') = 14

instance Ord Card where
  compare = comparing value

data Type = High | Pair | TwoPair | Three | Full | Four | Five deriving (Eq, Ord)

handType :: Hand -> Type
handType (Hand cards) = case counts of
  [5] -> Five
  [1, 4] -> Four
  [2, 3] -> Full
  [1, 1, 3] -> Three
  [1, 2, 2] -> TwoPair
  [1, 1, 1, 2] -> Pair
  _ -> High
  where
    counts :: [Int]
    counts = sort $ Map.elems $ foldr (\card map -> Map.insertWith (+) card 1 map) Map.empty cards

instance Ord Hand where
  compare = comparing (\x@(Hand cards) -> (handType x, cards))

parse :: String -> Parsed
parse = map (parseLine . words) . lines
  where
    parseLine [hand, bid] = (Hand (map Card hand), read bid)

part1 :: Parsed -> Int
part1 = sum . zipWith (*) [1 ..] . map snd . sort

part2 :: Parsed -> Int
part2 = undefined
