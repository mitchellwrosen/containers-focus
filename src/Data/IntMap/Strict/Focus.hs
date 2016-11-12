{-# language BangPatterns #-}
{-# language LambdaCase   #-}

module Data.IntMap.Strict.Focus
  ( -- * Focus
    focus
    -- * Re-exports
  , module Data.IntMap.Strict
  , module Focus
    -- | 'Decision'@(..), @'Strategy'
  ) where

import Data.IntMap.Internal
import Data.IntMap.Strict
import Focus (Decision(..), Strategy)

-- | 'focus' on a 'Key' in an 'IntMap', and decide whether to 'Keep', 'Replace',
-- or 'Remove' it.
--
-- ==== __Examples__
--
-- Here is how you might combine 'delete' and 'lookup' in one traversal of an
-- 'IntMap':
--
-- @
-- {-\# language TupleSections \#-}
--
-- deleteLookup :: 'Key' -> 'IntMap' a -> ('Maybe' a, 'IntMap' a)
-- deleteLookup = 'focus' (, 'Remove')
-- @
--
-- >>> deleteLookup 1 empty
-- (Nothing, fromList [])
--
-- >>> deleteLookup 1 (singleton 1 'a')
-- (Just 'a', fromList [])
focus :: Strategy a r -> Key -> IntMap a -> (r, IntMap a)
focus f !kx = go
 where
  go = \case
    t@(Bin p m l r)
      | nomatch kx p m ->
          let (z, decision) = f Nothing
          in case decision of
               Replace !x ->
                 let
                   !t' = link kx (Tip kx x) p t
                 in
                   (z, t')
               _ -> (z, t)
      | zero kx m ->
          let (z, t') = go l
              !t'' = binCheckLeft p m t' r
          in (z, t'')
      | otherwise ->
          let (z, t') = go r
              !t'' = binCheckRight p m l t'
          in (z, t'')

    t@(Tip ky y)
      | kx == ky ->
          let (z, decision) = f (Just y)
          in case decision of
               Keep       -> (z, t)
               Remove     -> (z, Nil)
               Replace !x -> (z, Tip kx x)
      | otherwise ->
          let
            (z, decision) = f Nothing
          in
            case decision of
              Replace !x ->
                let !t' = link kx (Tip kx x) ky t
                in (z, t')
              _ -> (z, t)

    Nil ->
      case decision of
        Replace !x -> (z, Tip kx x)
        _ -> (z, Nil)
     where
      (z, decision) = f Nothing
