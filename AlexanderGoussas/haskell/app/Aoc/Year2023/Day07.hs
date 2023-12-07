module Aoc.Year2023.Day07 where

import           Data.Char         (digitToInt)
import           Data.List         (group, sort)
import           Data.Maybe        (mapMaybe)
import           Data.Text         (Text)
import qualified Data.Text         as T

import           Control.Monad.Aoc


-- data Card = N Int | T | J | Q | K | A deriving (Show, Eq, Ord)
data Card = J | N Int | T | Q | K | A deriving (Show, Ord, Eq)
newtype Hand = Hand [Card] deriving (Show, Eq)

data Play = Play
  { hand :: !Hand
  , bid  :: {-# UNPACK #-} !Int
  }
  deriving (Show, Eq, Ord)

parsePlay :: Text -> Maybe Play
parsePlay text =
  case T.splitOn " " text of
    (h:b:_) -> Play <$> parseHand h <*> parseBid b
    _       -> Nothing
  where
    parseBid = return . read . T.unpack
    parseHand = return . Hand . map parseCard . T.unpack

    parseCard 'A' = A
    parseCard 'K' = K
    parseCard 'Q' = Q
    parseCard 'J' = J
    parseCard 'T' = T
    parseCard n   = N (digitToInt n)

jokers :: [Card] -> Int
jokers = length . filter (== J)
{-# INLINE jokers #-}

maximumOr :: Int -> [Int] -> Int
maximumOr n [] = n
maximumOr _ xs = maximum xs
{-# INLINE maximumOr #-}

largestEqual :: [Card] -> Int
largestEqual = maximumOr 0 . map length . group . sort . filter (/= J)
{-# INLINE largestEqual #-}

fiveOfKind :: Hand -> Int -> Bool
fiveOfKind (Hand h) !largest = largest == 5 || largest + jokers h >= 5
{-# INLINE fiveOfKind #-}

fourOfKind :: Hand -> Int -> Bool
fourOfKind (Hand h) !largest = largest == 4 || largest + jokers h >= 4
{-# INLINE fourOfKind #-}

fullHouse :: Hand -> Bool
fullHouse (Hand cards) =
  let mapper c = map (\x -> if c == x then J else x) cards in
  let !possibilities = cards : map mapper cards in
  let !lengths = map (sort . map length . group . sort) possibilities in
  [2, 3] `elem` lengths
{-# INLINE fullHouse #-}

threeOfKind :: Hand -> Int -> Bool
threeOfKind (Hand h) !largest = largest == 3 || largest + jokers h >= 3
{-# INLINE threeOfKind #-}

twoPairs :: Hand -> Bool
twoPairs (Hand h) =
  let !pairs = length . filter (==2) . map length . group . sort $ h in
  let !jokers' = jokers h in
  pairs == 2 || pairs + jokers' >= 2 || jokers' >= 2
{-# INLINE twoPairs #-}

onePair :: Hand -> Int -> Bool
onePair (Hand h) !largest = largest == 2 || largest + jokers h >= 2
{-# INLINE onePair #-}

handType :: Hand -> Int
handType hand@(Hand cards) =
  let !largest = largestEqual cards in
  if fiveOfKind hand largest then 9
  else if fourOfKind hand largest then 8
  else if fullHouse hand then 7
  else if threeOfKind hand largest then 6
  else if twoPairs hand then 5
  else if onePair hand largest then 4
  else 0
{-# INLINE handType #-}

instance Ord Hand where
  compare h1@(Hand cards1) h2@(Hand cards2) =
    let t1 = handType h1 in
    let t2 = handType h2 in
    if t1 == t2
    then compare cards1 cards2
    else compare t1 t2

rankPlays :: [Play] -> [(Int, Play)]
rankPlays = zip [1..] . sort
{-# INLINE rankPlays #-}

score :: (Int, Play) -> Int
score (rank, Play _ bid) = rank * bid
{-# INLINE score #-}

instance MonadAoc 7 2023 where
  type Result 7 2023 = Int
  partOne _ _ = sum . map score . rankPlays . mapMaybe parsePlay . T.lines <$> getInput
  partTwo _ _ = sum . map score . rankPlays . mapMaybe parsePlay . T.lines <$> getInput
