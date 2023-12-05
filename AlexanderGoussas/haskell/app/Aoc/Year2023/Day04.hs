module Aoc.Year2023.Day04 where

import           Data.Attoparsec.Text hiding (take)
import           Data.List            (foldl', intersect)
import           Data.Text            (Text)

import           Control.Monad.Aoc
import           Control.Monad.Cache

data Card = Card
  { cardId             :: {-# UNPACK #-}!Int
  , cardNumbers        :: ![Int]
  , cardWinningNumbers :: ![Int]
  }
  deriving (Eq, Show, Ord)

spaces :: Parser ()
spaces = many' space *> pure ()

numbersParser :: Parser [Int]
numbersParser = decimal `sepBy'` spaces

cardParser :: Parser Card
cardParser = Card
  <$> (string "Card" *> spaces *> decimal <* char ':' <* spaces)
  <*> (numbersParser <* spaces <* char '|' <* spaces)
  <*> numbersParser

cardsParser :: Parser [Card]
cardsParser = cardParser `sepBy'` endOfLine

parseCards :: Text -> [Card]
parseCards = either error id . parseOnly cardsParser

points :: [Double]
points = 1:[ 2**x | x <- [1..]]

points' :: Int -> Double
points' 0 = 0
points' n = go (n-1) 1
  where
    go 0 !acc = acc
    go n !acc = go (n - 1) (acc * 2)
{-# INLINE points' #-}

countCards :: [Card] -> Int
countCards cards = runCache $ go cards cards 0
  where
    go [] _ !n = return n
    go ((Card {..}):cards) originals !n = do
      wonCopies <- getCache cardId
      case wonCopies of
        Just wonCopies -> go (wonCopies ++ cards) originals (n + 1)
        Nothing ->
          let !winningNumbers = length $ intersect cardWinningNumbers cardNumbers in
          let !wonCopies = take winningNumbers $ drop cardId originals in
          putCache cardId wonCopies >> go (wonCopies ++ cards) originals (n + 1)

instance MonadAoc 4 2023 where
  type Result 4 2023 = Double

  partOne _ _ = do
    input <- getInput
    let cards = parseCards input
    let numbers = map (\(Card {..}) -> cardWinningNumbers `intersect` cardNumbers) cards
    return $! foldl' (\acc ns -> acc + points' (length ns)) 0 numbers

  partTwo _ _ = do
    input <- getInput
    let cards = parseCards input
    let total = countCards cards
    return $ fromIntegral total
