module Aoc.Year2023.Day05 where

import           Control.Concurrent.Async
import           Control.Monad.IO.Class   (liftIO)
import           Data.List                (foldl')
import           Data.List.Split          (chunksOf)
import           Data.Maybe               (fromJust, fromMaybe)
import           Data.Text                (Text)
import qualified Data.Text                as T

import           Control.Monad.Aoc

type Seed = Int

data Range = Range
  { rangeDestStart :: !Int
  , rangeSrcStart  :: !Int
  , rangeLength    :: !Int
  }
  deriving Show

rangeMapping :: Range -> Int -> Maybe Int
rangeMapping (Range {..}) seed =
  if seed >= rangeSrcStart && seed < rangeSrcStart + rangeLength
  then Just $ rangeDestStart + (seed - rangeSrcStart)
  else Nothing

parseRange :: Text -> Range
parseRange text =
  let [destStart, srcStart, length] = map (read . T.unpack) $ T.splitOn " " text in
  Range destStart srcStart length

parseInput :: Text -> ([Seed], [[Range]])
parseInput text =
  let groups = T.splitOn "\n\n" text in
  let seeds = map (read . T.unpack) $ T.splitOn " " $ fromJust $ T.stripPrefix "seeds: " $ head groups in
  let mappings = map (filter (not . T.null) . tail . T.splitOn "\n") $ tail groups in
  let ranges = map (map parseRange) mappings in
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

lowestLocationForSeeds :: Seed -> Seed -> [[Range]] -> Int
lowestLocationForSeeds firstSeed lastSeed ranges =
  go firstSeed lastSeed ranges maxBound
  where
    go !currentSeed !lastSeed _ !lowest | currentSeed >= lastSeed = lowest
    go !currentSeed !lastSeed ranges !lowest =
      let !result = lowestLocationForSeed currentSeed ranges in
      go (currentSeed + 1) lastSeed ranges $ min lowest result

instance MonadAoc 5 2023 where
  type Result 5 2023 = Int

  partOne _ _ = do
    input <- getInput
    let (seeds, ranges) = parseInput input
    let lowest = minimum $ map (`lowestLocationForSeed` ranges) seeds
    return lowest

  partTwo _ _ = do
    input <- getInput
    let (seeds, ranges) = parseInput input
    let seedRanges = map (\[start, length] -> (start, start + length)) $ chunksOf 2 seeds
    let quarterSeeds = length seeds `div` 4
    let calculateLowestForSeedRanges = foldl' (\lowest (start, end) -> min lowest $ lowestLocationForSeeds start end ranges) maxBound
    let xs = take quarterSeeds seedRanges
    let ys = take quarterSeeds $ drop quarterSeeds seedRanges
    let zs = take quarterSeeds $ drop (2 * quarterSeeds) seedRanges
    let ws = drop (3 * quarterSeeds) seedRanges
    liftIO $ minimum <$> mapConcurrently (return . calculateLowestForSeedRanges) [xs, ys, zs, ws]
