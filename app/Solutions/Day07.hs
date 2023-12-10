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

data Type = High | Pair | TwoPair | Three | Full | Four | Five deriving (Eq, Ord, Enum)

countType [5] = Five
countType [1, 4] = Four
countType [2, 3] = Full
countType [1, 1, 3] = Three
countType [1, 2, 2] = TwoPair
countType [1, 1, 1, 2] = Pair
countType [1, 1, 1, 1, 1] = High

parse :: String -> Parsed
parse = map (parseLine . words) . lines
  where
    parseLine [hand, bid] = (Hand (map Card hand), read bid)

solve comp = sum . zipWith (*) [1 ..] . map snd . sortBy comp

part1 :: Parsed -> Int
part1 = solve comp
  where
    comp (a, _) (b, _) = comparing (\x@(Hand cards) -> (handType x, cards)) a b
    handType :: Hand -> Type
    handType (Hand cards) = countType counts
      where
        counts :: [Int]
        counts = sort $ Map.elems $ foldr (\card map -> Map.insertWith (+) card 1 map) Map.empty cards

part2 :: Parsed -> Int
part2 = solve comp
  where
    comp (a, _) (b, _) = comparing (\x@(Hand cards) -> (handType x, map value2 cards)) a b
    value2 (Card 'J') = 1
    value2 x = value x

    handType :: Hand -> Type
    handType (Hand cards) = countType newCounts
      where
        counts :: [Int]
        counts = sort $ Map.elems $ foldr (\card map -> Map.insertWith (+) card 1 map) Map.empty (filter (/= Card 'J') cards)
        jokerCount = length $ filter (== Card 'J') cards
        newCounts = if null counts then [5] else init counts ++ [last counts + jokerCount]
