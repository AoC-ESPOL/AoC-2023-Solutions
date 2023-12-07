module Aoc.Year2023.Day06 where

import           Control.Monad.IO.Class (liftIO)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Debug.Trace            as Debug

import           Control.Monad.Aoc

data Race = Race
  { raceTime :: !Int
  , raceDist :: !Int
  }
  deriving (Show, Eq)

parseRaces :: Text -> [Race]
parseRaces text =
  let lines = T.lines text in
  let [times, distances] = map ((map (read . T.unpack) . filter (not . T.null)) . drop 1 . T.splitOn " ") lines in
  zipWith Race times distances
{-# INLINE parseRaces #-}

parseSingleRace :: Text -> Race
parseSingleRace text =
  let lines = T.lines text in
  let [time, distance] = map (read . T.unpack . mconcat . drop 1 . T.splitOn " ") lines in
  Race time distance
{-# INLINE parseSingleRace #-}

numberOfWaysToBeatRecord :: Race -> Int
numberOfWaysToBeatRecord (Race {..}) = go raceTime raceDist 0 0
  where
    -- TODO: This is a bit of a hack, but it works
    delta = 1

    go !time !recordDist !ways !millis
      | millis >= time = ways
      | millis < time && millis * (time - millis) > recordDist =
          go time recordDist (ways + delta) (millis + delta)
      | otherwise = go time recordDist ways (millis + delta)

instance MonadAoc 6 2023 where
  type Result 6 2023 = Int

  partOne _ _ = do
    input <- getInput
    let races = parseRaces input
    let ways = map numberOfWaysToBeatRecord races
    return $ product ways

  partTwo _ _ = do
    input <- getInput
    let race = parseSingleRace input
    let ways = numberOfWaysToBeatRecord race
    return ways
