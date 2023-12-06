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

parseSingleRace :: Text -> Race
parseSingleRace text =
  let lines = T.lines text in
  let [time, distance] = map (read . T.unpack . mconcat . drop 1 . T.splitOn " ") lines in
  Race time distance

numberOfWaysToBeatRecord :: Race -> Int
numberOfWaysToBeatRecord (Race {..}) = go raceTime raceDist 0 0
  where
    go time _ !ways !millis | millis >= time = ways
    go time recordDist !ways !millis | millis < time =
      let !dist = millis * (time - millis) in
      if dist > recordDist
      then go time recordDist (ways + 1) (millis + 1)
      else go time recordDist ways (millis + 1)

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
