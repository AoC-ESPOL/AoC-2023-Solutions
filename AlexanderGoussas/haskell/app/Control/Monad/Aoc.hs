module Control.Monad.Aoc
(
    AocData(..)
  , Aoc
  , MonadAoc(..)
  , runAoc
  , getInput
  , day
  , year
)
where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader, ReaderT, asks, runReaderT)
import           Data.Proxy
import           Data.Text              (Text)
import           GHC.TypeLits

data AocData = AocData
  { aocInput :: !Text
  }

newtype Aoc a = Aoc { unAoc :: ReaderT AocData IO a }
  deriving newtype (Functor, Applicative, Monad, MonadReader AocData, MonadIO)

class MonadAoc (day :: Nat) (year :: Nat) where
  type Result day year
  partOne :: Proxy day -> Proxy year -> Aoc (Result day year)
  partTwo :: Proxy day -> Proxy year -> Aoc (Result day year)

day :: (KnownNat day) => Proxy day
day = Proxy

year :: (KnownNat year) => Proxy year
year = Proxy

-- | Get the input for a given day.
getInput :: Aoc Text
getInput = asks aocInput

-- | Run a computation in the Aoc monad.
runAoc :: AocData -> Aoc a -> IO a
runAoc aocData aoc = runReaderT (unAoc aoc) aocData
