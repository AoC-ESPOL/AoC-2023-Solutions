{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Aoc.Year2023.Day05 where

import           Control.Monad      (guard)
import           Data.Coerce        (coerce)
import           Data.Function      (on)
import           Data.List          (minimumBy, sortOn)
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.List.Split    (chunksOf)
import           Data.Maybe         (fromJust, fromMaybe)
import           Data.Semigroup     (sconcat)
import           Data.Text          (Text)
import qualified Data.Text          as T

import           Control.Monad.Aoc

type Seed = Int

data Range = Range
  { rangeDestStart :: !Int
  , rangeSrcStart  :: !Int
  , rangeLength    :: !Int
  }
  deriving Show

newtype RangeMap = RangeMap { getRanges :: [Range] }
  deriving Show

instance Semigroup RangeMap where
  RangeMap xs <> RangeMap ys = RangeMap $ do
    Range destA srcA lenA <- xs
    Range destB srcB lenB <- ys
    let dest = destB + max 0 (destA - srcB)
    let src  = srcA + max 0 (srcB - destA)
    let len  = min (destA + lenA) (srcB + lenB) - max destA srcB
    guard $  len > 0
    return $ Range dest src len

seedsToRangeMap :: [Seed] -> RangeMap
seedsToRangeMap seeds = RangeMap $
  let !pairwise = chunksOf 2 seeds in
  let !ranges = map (\[start, length] -> Range start start length) pairwise in
  ranges

saturateRangeMap :: RangeMap -> RangeMap
saturateRangeMap (RangeMap ranges) =
  RangeMap $ go (sortOn rangeSrcStart ranges) 0 []
  where
    go [] !start saturated = Range start start (maxBound - start) : saturated
    go (range:ranges) !start saturated =
      let previous = Range start start (rangeSrcStart range - start) in
      let next = rangeSrcStart range + rangeLength range in
      if rangeLength previous > 0
      then go ranges next (previous : range : saturated)
      else go ranges next (range : saturated)

rangeMapping :: Range -> Int -> Maybe Int
rangeMapping (Range {..}) seed =
  if seed >= rangeSrcStart && seed < rangeSrcStart + rangeLength
  then Just $ rangeDestStart + (seed - rangeSrcStart)
  else Nothing

parseRange :: Text -> Range
parseRange text =
  let [destStart, srcStart, length] = map (read . T.unpack) $ T.splitOn " " text in
  Range destStart srcStart length

parseInput :: Text -> ([Seed], [RangeMap])
parseInput text =
  let groups = T.splitOn "\n\n" text in
  let seeds = map (read . T.unpack) $ T.splitOn " " $ fromJust $ T.stripPrefix "seeds: " $ head groups in
  let mappings = map (filter (not . T.null) . tail . T.splitOn "\n") $ tail groups in
  let ranges = map (RangeMap . map parseRange) mappings in
  (seeds, ranges)

lowestLocationForSeed :: Seed -> [[Range]] -> Int
lowestLocationForSeed seed ranges = go seed ranges
  where
    go seed []            = seed
    go seed (ranges:rest) = go (go' seed ranges) rest
    {-# INLINE go #-}

    go' seed []           = seed
    go' seed (range:rest) = fromMaybe (go' seed rest) (rangeMapping range seed)
    {-# INLINE go' #-}

instance MonadAoc 5 2023 where
  type Result 5 2023 = Int

  partOne _ _ = do
    input <- getInput
    let (seeds, ranges) = parseInput input
    let lowest = minimum $ map (flip lowestLocationForSeed $ map getRanges ranges) seeds
    return lowest

  partTwo _ _ = do
    input <- getInput
    let (seeds, ranges) = parseInput input
    let !ranges' = sconcat $ seedsToRangeMap seeds :| map saturateRangeMap ranges
    return $ rangeDestStart . minimumBy (compare `on` rangeDestStart) . coerce @RangeMap @[Range] $ ranges'
