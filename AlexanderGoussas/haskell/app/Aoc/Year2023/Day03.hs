module Aoc.Year2023.Day03 (parseInput) where

import           Control.Monad     (guard)
import           Data.Char         (isDigit)
import           Data.Either       (fromRight)
import           Data.List         (foldl')
import           Data.Map          (Map, (!), (!?))
import qualified Data.Map          as Map
import           Data.Maybe        (mapMaybe)
import           Data.Text         (Text)
import qualified Data.Text         as T
import qualified Data.Text.Read    as TR
import qualified Data.Text.Unsafe  as TU

import           AocLib
import           Control.Monad.Aoc

neighbors :: Int -> Int -> [(Int, Int)]
neighbors x y = do
  dx <- [-1, 0, 1]
  dy <- [-1, 0, 1]
  guard $ (dx, dy) /= (0, 0)
  return (x + dx, y + dy)
{-# INLINE neighbors #-}

isSymbol :: Text -> Bool
isSymbol text = isSymbol' $ TU.unsafeHead text
{-# INLINE isSymbol #-}

isSymbol' :: Char -> Bool
isSymbol' c = c /= '.' && not (isDigit c)
{-# INLINE isSymbol' #-}

parseInput :: Text -> Map (Int, Int) Text
parseInput text = go Map.empty 0 (T.lines text)
  where
    go m _ [] = m
    go m !linum (line:lines) =
      let !stuff = splitKeepingBy (\c -> isSymbol' c || c == '.') (T.unpack line) in
      let !stuffWithOffset = zip (scanl1 (+) $ 0 : map length stuff) stuff in
      let !cleanStuff = filter ((/=".") . snd) stuffWithOffset in
      let !m' = foldl' (\m (offset, text) -> Map.insert (offset, linum) (T.pack text) m) m cleanStuff in
      go m' (linum + 1) lines
    {-# INLINE go #-}

sumAdjacentParts :: Map (Int, Int) Text -> Int
sumAdjacentParts parts = go parts (Map.keys parts) 0
  where
    go _ [] !acc = acc
    go parts ((x, y):coords) !acc =
      let !text = parts ! (x, y) in
      if isSymbol text
      then go parts coords acc
      else
        let digits = [(x, y) | x <- [x .. x + T.length text - 1]] in
        let adjacent = concatMap (filter isSymbol . mapMaybe (parts !?) . uncurry neighbors) digits in
        let acc' = if null adjacent then acc else acc + fst (fromRight undefined $ TR.decimal text) in
        go parts coords acc'

multiplyGears :: Map (Int, Int) Text -> Int
multiplyGears parts = sum $ go parts (Map.keys parts) []
  where
    isAdjacentTo m x y coords@(x', y') =
      let text = m ! coords in
      let coords = [(x', y') | x' <- [x' .. x' + T.length text - 1]] in
      isAdjacentTo' x y coords

    isAdjacentTo' _ _ [] = False
    isAdjacentTo' x y ((x',y'):coords) =
      if abs (x' - x) <= 1 && abs (y' - y) <= 1
      then True
      else isAdjacentTo' x y coords

    isNumeric = all isDigit . T.unpack

    go :: Map (Int, Int) Text -> [(Int, Int)] -> [Int] -> [Int]
    go _ [] acc = acc
    go parts ((x, y):coords) acc =
      let text = parts ! (x, y) in
      if text /= "*"
      then go parts coords acc
      else
        let adjacent
              = map (read @Int . T.unpack)
              . map (\coord -> parts ! coord)
              $ filter (\coord -> isNumeric (parts ! coord) && isAdjacentTo parts x y coord) (Map.keys parts)
        in
        if length adjacent == 2
        then go parts coords (product adjacent : acc)
        else go parts coords acc

instance MonadAoc 3 2023 where
  type Result 3 2023 = Int
  partOne _ _ = sumAdjacentParts . parseInput <$> getInput
  partTwo _ _ = multiplyGears . parseInput <$> getInput
