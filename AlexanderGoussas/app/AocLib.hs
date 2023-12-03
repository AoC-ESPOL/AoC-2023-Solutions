module AocLib
(
    splitKeepingBy
  , maybeMap
)
where

import           Data.List.Split


-- | Split the provided list where the predicate returns True,
-- keeping the separators in the result.
splitKeepingBy :: (a -> Bool) -> [a] -> [[a]]
splitKeepingBy f = split (dropInitBlank . dropFinalBlank . dropInnerBlanks $ whenElt f)
{-# INLINE splitKeepingBy #-}

-- | Conditionally fmap over a Maybe.
maybeMap :: (a -> Bool) -> (a -> b) -> Maybe a -> Maybe b
maybeMap pred f (Just x) | pred x = pure $ f x
maybeMap _ _ _ = Nothing
{-# INLINE maybeMap #-}
