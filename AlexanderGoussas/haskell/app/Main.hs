module Main where

import           Control.Monad.IO.Class (liftIO)

import           GHC.TypeLits

import qualified Data.Text.IO           as TIO
import           Options.Applicative

import           Aoc.Year2023.Day01     ()
import           Aoc.Year2023.Day02     ()
import           Aoc.Year2023.Day03     ()
import           Aoc.Year2023.Day04     ()
import           Aoc.Year2023.Day05     ()
import           Aoc.Year2023.Day06     ()
import           Control.Monad.Aoc


data Options = Options
  { optDay  :: !Int
  , optYear :: !Int
  , optPart :: !Int
  }
  deriving Show

parseOpts :: Parser Options
parseOpts = Options
  <$> option auto
    ( short 'd'
    <> long "day"
    <> help "The puzzle day"
    <> metavar "INT" )
  <*> option auto
    ( short 'y'
    <> long "year"
    <> help "The puzzle year"
    <> metavar "INT" )
  <*> option auto
    ( short 'p'
    <> long "part"
    <> help "The puzzle part"
    <> metavar "INT" )

opts :: IO Options
opts = execParser opts'
  where
    opts' = info (parseOpts <**> helper)
      ( fullDesc
      <> progDesc "Run the specified AoC puzzle"
      <> header "aoc - AoC puzzle runner" )

outStr :: Show a => a -> Aoc ()
outStr = liftIO . print

data Puzzle (day :: Nat) (year :: Nat) where
  Y2023D01 :: Puzzle 1 2023
  Y2023D02 :: Puzzle 2 2023
  Y2023D03 :: Puzzle 3 2023
  Y2023D04 :: Puzzle 4 2023
  Y2023D05 :: Puzzle 5 2023
  Y2023D06 :: Puzzle 6 2023

data SomePuzzle =
  forall day year.
  ( MonadAoc day year
  , KnownNat day
  , KnownNat year
  , Show (Result day year)
  ) => SomePuzzle (Puzzle day year)

puzzle :: Options -> SomePuzzle
puzzle (Options 1 2023 _ ) = SomePuzzle Y2023D01
puzzle (Options 2 2023 _)  = SomePuzzle Y2023D02
puzzle (Options 3 2023 _)  = SomePuzzle Y2023D03
puzzle (Options 4 2023 _)  = SomePuzzle Y2023D04
puzzle (Options 5 2023 _)  = SomePuzzle Y2023D05
puzzle (Options 6 2023 _)  = SomePuzzle Y2023D06
puzzle _                   = error "Invalid puzzle day and year"

runPuzzle
  :: forall day year.
  ( MonadAoc day year
  , KnownNat day
  , KnownNat year
  , Show (Result day year)
  ) => Puzzle day year -> Int -> IO ()
runPuzzle _ part = do
  input <- TIO.readFile "input06.txt"
  runAoc (AocData input) $ do
    case part of
      1 -> outStr =<< partOne (day @day) (year @year)
      2 -> outStr =<< partTwo (day @day) (year @year)
      _ -> error "Invalid puzzle part"

main :: IO ()
main = opts >>= \opts' ->
  case puzzle opts' of
    (SomePuzzle puzzle) -> runPuzzle puzzle (optPart opts')
