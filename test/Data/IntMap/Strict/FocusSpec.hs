{-# language OverloadedLists #-}

module Data.IntMap.Strict.FocusSpec where

import Control.Exception (evaluate)
import Data.IntMap.Strict.Focus
import Test.Hspec

spec :: Spec
spec = do
  describe "focus" $ do
    it "can insert" $ do
      let f Nothing  = (False, Replace 'a')
          f (Just _) = (True,  Replace 'b')
      list (focus f 1 mempty)
        `shouldBe` (False, [(1, 'a')])

    it "can replace" $ do
      let f Nothing  = (False, Replace 'a')
          f (Just _) = (True,  Replace 'b')
      list (focus f 1 [(1, 'x')])
        `shouldBe` (True, [(1, 'b')])

    it "can lookup Nothing" $ do
      let f Nothing  = (False, Keep)
          f (Just _) = (True,  Keep)
      list (focus f 1 mempty)
        `shouldBe` (False, [])

    it "can lookup Just" $ do
      let f Nothing  = (False, Keep)
          f (Just _) = (True,  Keep)
      list (focus f 1 [(1, 'a')])
        `shouldBe` (True, [(1, 'a')])

    it "can delete Nothing" $ do
      let f Nothing  = (False, Remove)
          f (Just _) = (True,  Remove)
      list (focus f 1 mempty)
        `shouldBe` (False, [])

    it "can delete Just" $ do
      let f Nothing  = (False, Remove)
          f (Just _) = (True,  Remove)
      list (focus f 1 [(1, 'a')])
        `shouldBe` (True, [])

    it "is strict in keys" $ do
      let f _ = ((), Replace undefined)
      evaluate (seq (focus f 1 mempty) ())
        `shouldThrow` errorCall "Prelude.undefined"

list :: (a, IntMap Char) -> (a, [(Int, Char)])
list (a, b) = (a, toList b)
