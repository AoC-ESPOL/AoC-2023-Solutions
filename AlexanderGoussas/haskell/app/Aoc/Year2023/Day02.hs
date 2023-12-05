module Aoc.Year2023.Day02 () where

import           Data.List            (foldl')
import           Data.Map             ((!))
import qualified Data.Map             as Map
import qualified Data.Vector.Unboxed  as VU

import           Data.Attoparsec.Text

import           Control.Monad.Aoc

data CubeColor = Red | Green | Blue
  deriving (Eq, Show, Ord)

data Cube = Cube
  { cubeAmount :: {-# UNPACK #-} !Int
  , cubeColor  :: !CubeColor
  }
  deriving (Eq, Show)

data Game = Game
  { gameId    :: {-# UNPACK #-} !Int
  , gameCubes :: ![[Cube]]
  }
  deriving (Eq, Show)

parseCube :: Parser Cube
parseCube = Cube <$> decimal <*> (char ' ' *> parseColor)
  where
    parseColor = choice
      [ Red   <$ string "red"
      , Green <$ string "green"
      , Blue  <$ string "blue"
      ]

parseGame :: Parser Game
parseGame = Game <$> (string "Game " *> decimal <* char ':') <*> (gameSubset `sepBy'` char ';')
  where
    gameSubset = char ' ' *> parseCube `sepBy'` string ", "

parseGames :: Parser [Game]
parseGames = parseGame `sepBy'` endOfLine

selectGames :: [Game] -> [Int]
selectGames
  = map fst
  . filter (\(_, m) -> (m ! Red) .<=. 12 && (m ! Blue) .<=. 13 && (m ! Green) .<=. 14)
  . map (fmap $! foldl' upsertCube Map.empty)
  . map (\game -> (gameId game, concat $! gameCubes game))
  where
    (.<=.) !amounts !n = all (<= n) amounts
    {-# INLINE (.<=.) #-}

    upsertCube m cube =
      let !amount = cubeAmount cube in
      Map.alter (Just . maybe [amount] (amount:)) (cubeColor cube) m

{-# INLINE selectGames #-}

selectGames' :: [Game] -> Int
selectGames'
  = foldl' (\acc v -> acc + VU.product v) 1
  . map ( Map.foldr chooseGreatest VU.empty
        . foldl' upsertCube Map.empty
        . concat
        . gameCubes
        )
  where
    chooseGreatest :: VU.Vector Int -> VU.Vector Int -> VU.Vector Int
    chooseGreatest amounts !chosen = VU.maximum amounts `VU.cons` chosen
    {-# INLINE chooseGreatest #-}

    upsertCube m cube =
      let !amount = cubeAmount cube in
      Map.alter (Just . maybe (VU.singleton amount) (VU.cons amount)) (cubeColor cube) m
    {-# INLINE upsertCube #-}

{-# INLINE selectGames' #-}

instance MonadAoc 2 2023 where
  type Result 2 2023 = Int

  partOne _ _ = do
    input <- getInput
    case parseOnly parseGames input of
      Left err    -> error err
      Right games -> return $! sum $ selectGames games

  partTwo _ _ = do
    input <- getInput
    case parseOnly parseGames input of
      Left err    -> error err
      Right games -> return $! selectGames' games
