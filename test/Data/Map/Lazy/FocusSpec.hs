{-# language OverloadedLists            #-}
{-# options_ghc -fno-warn-type-defaults #-}

module Data.Map.Lazy.FocusSpec where

import Data.Map.Lazy.Focus
import Test.Hspec

spec :: Spec
spec = do
  describe "focus" $ do
    it "can insert" $ do
      let f Nothing  = (False, Replace 1)
          f (Just _) = (True,  Replace 2)
      list (focus f 'a' mempty)
        `shouldBe` (False, [('a', 1)])

    it "can replace" $ do
      let f Nothing  = (False, Replace 1)
          f (Just _) = (True,  Replace 2)
      list (focus f 'a' [('a', 0)])
        `shouldBe` (True, [('a', 2)])

    it "can lookup Nothing" $ do
      let f Nothing  = (False, Keep)
          f (Just _) = (True,  Keep)
      list (focus f 'a' mempty)
        `shouldBe` (False, [])

    it "can lookup Just" $ do
      let f Nothing  = (False, Keep)
          f (Just _) = (True,  Keep)
      list (focus f 'a' [('a', 1)])
        `shouldBe` (True, [('a', 1)])

    it "can delete Nothing" $ do
      let f Nothing  = (False, Remove)
          f (Just _) = (True,  Remove)
      list (focus f 'a' mempty)
        `shouldBe` (False, [])

    it "can delete Just" $ do
      let f Nothing  = (False, Remove)
          f (Just _) = (True,  Remove)
      list (focus f 'a' [('a', 1)])
        `shouldBe` (True, [])

    it "is lazy in keys" $ do
      let f _ = ((), Replace undefined)
      seq (focus f 'a' mempty) ()
        `shouldBe` ()

list :: (a, Map Char Int) -> (a, [(Char, Int)])
list (a, b) = (a, toList b)
