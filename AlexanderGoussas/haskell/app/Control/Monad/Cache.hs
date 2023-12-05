module Control.Monad.Cache where

import           Control.Monad.State
import           Data.Map            (Map)
import qualified Data.Map            as Map

newtype Cache k v a = Cache { unCache :: State (Map k v) a }
  deriving newtype (Functor, Applicative, Monad, MonadState (Map k v))

-- | Run a cached computation.
runCache :: Cache k v a -> a
runCache cache = evalState (unCache cache) Map.empty
{-# INLINE runCache #-}

-- | Get a value from the cache.
getCache :: Ord k => k -> Cache k v (Maybe v)
getCache k = gets (Map.lookup k)
{-# INLINE getCache #-}

-- | Put a value in the cache.
putCache :: Ord k => k -> v -> Cache k v ()
putCache k v = modify' (Map.insert k v)
{-# INLINE putCache #-}
