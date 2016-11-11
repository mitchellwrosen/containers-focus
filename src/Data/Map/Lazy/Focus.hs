{-# language BangPatterns #-}
{-# language CPP          #-}

module Data.Map.Lazy.Focus
  ( -- * Focus
    focus
    -- * Re-exports
  , module Data.Map.Lazy
  , module Focus
    -- | 'Decision'@(..), @'Strategy'
  ) where

import PtrEquality (ptrEq)

import Data.Map.Internal (Map(..), balanceL, balanceR, glue, singleton)
import Data.Map.Lazy
import Focus             (Decision(..), Strategy)

-- | 'focus' on a key @k@ in a 'Map', and decide whether to 'Keep', 'Replace',
-- or 'Remove' it.
--
-- ==== __Examples__
--
-- Here is how you might combine 'delete' and 'lookup' in one traversal of a
-- 'Map':
--
-- @
-- {-\# language TupleSections \#-}
--
-- deleteLookup :: 'Ord' k => k -> 'Map' k a -> ('Maybe' a, 'Map' k a)
-- deleteLookup = 'focus' (, 'Remove')
-- @
--
-- >>> deleteLookup 'a' empty
-- (Nothing, fromList [])
--
-- >>> deleteLookup 'a' (singleton 'a' 5)
-- (Just 5, fromList [])
focus :: Ord k => Strategy a r -> k -> Map k a -> (r, Map k a)
focus f = go
 where
  go !kx t@Tip =
    case decision of
      Replace x ->
        let !t' = singleton kx x
        in (z, t')
      _ -> (z, t)
   where
    (z, decision) = f Nothing

  go kx t@(Bin sz ky y l r) =
    case compare kx ky of
      LT | l `ptrEq` l' -> (z, t)
         | otherwise ->
             let !t' = balanceL ky y l' r
             in (z, t')
       where
        !(z, !l') = go kx l

      GT | r `ptrEq` r' -> (z, t)
         | otherwise ->
             let !t' = balanceR ky y l r'
             in (z, t')
       where
        !(z, !r') = go kx r

      EQ ->
        case decision of
          Keep -> (z, t)
          Remove ->
            let !t' = glue l r
            in (z, t')
          Replace x
            | kx `ptrEq` ky && x `ptrEq` y -> (z, t)
            | otherwise -> (z, Bin sz kx x l r)
         where
          (z, decision) = f (Just y)
#if __GLASGOW_HASKELL__
{-# INLINABLE focus #-}
#else
{-# INLINE focus #-}
#endif
