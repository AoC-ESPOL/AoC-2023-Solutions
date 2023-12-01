module Aoc.Year2023.Day01 where

import           Data.Char            (digitToInt, isDigit)
import           Data.Maybe           (mapMaybe)

import           Data.Attoparsec.Text
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Unsafe     as TU

import           Control.Monad.Aoc

sample :: [Text]
sample =
  [ "two1nine"
  , "eightwothree"
  , "abcone2threexyz"
  , "xtwone3four"
  , "4nineeightseven2"
  , "zoneight234"
  , "7pqrstsixteen"
  ]

firstDigit :: Text -> Int
firstDigit = digitToInt . T.head . T.dropWhile (not . isDigit)

lastDigit :: Text -> Int
lastDigit = digitToInt . T.head . T.dropWhile (not . isDigit) . T.reverse

spelledOutDigit :: Parser Int
spelledOutDigit = choice
  [ string "one"   *> pure 1
  , string "eight" *> pure 8
  , string "nine"  *> pure 9
  , string "two"   *> pure 2
  , string "four"  *> pure 4
  , string "three" *> pure 3
  , string "six"   *> pure 6
  , string "five"  *> pure 5
  , string "seven" *> pure 7
  ]

digit' :: Parser Int
digit' = choice [spelledOutDigit , digitToInt <$> digit]

parseDigit :: Text -> Maybe Int
parseDigit = maybeResult . parse digit'

validDigits :: [Text]
validDigits = map (T.pack . show) [1..9] ++ ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
{-# INLINE validDigits #-}

isStartOfDigit :: Text -> Bool
isStartOfDigit !text = any (T.isPrefixOf text) validDigits
{-# INLINE isStartOfDigit #-}

parseFirst :: Text -> Maybe Int
parseFirst !text = go text 0
  where
    go _ !i | i == T.length text = Nothing
    go !text !i = go' text i 1

    go' !text !i !size | i + size > T.length text = go text (i + 1)
    go' !text !i !size =
      let substring = TU.takeWord16 size $ TU.dropWord16 i text in
      case parseDigit substring of
        Just digit                         -> Just digit
        Nothing | isStartOfDigit substring -> go' text i (size + 1)
        Nothing                            -> go text (i + 1)
{-# INLINE parseFirst #-}

parseLast :: Text -> Maybe Int
parseLast text = go text (T.length text)
  where
    go _ !i | i < 0 = Nothing
    go !text !i = go' text i 1

    go' !text !i !size | i - size < 0 = go text (i - 1)
    go' !text !i !size =
      let substring = TU.takeWord16 size $ TU.dropWord16 (i - size) text in
      case parseDigit substring of
        Just digit -> Just digit
        Nothing    -> go text (i + 1)
{-# INLINE parseLast #-}

parseFirstAndLast :: Text -> Maybe (Int, Int)
parseFirstAndLast text = (,) <$> parseFirst text <*> parseLast text
{-# INLINE parseFirstAndLast #-}

instance MonadAoc 1 2023 where
  type Result 1 2023 = Int

  partOne _ _ = do
    input <- getInput
    return
      . sum
      . map (\line -> firstDigit line * 10 + lastDigit line)
      $ T.lines input

  partTwo _ _ = do
    input <- getInput
    return
      $! sum
      . map (\(x, y) -> x * 10 + y)
      . mapMaybe parseFirstAndLast
      $ T.lines input
