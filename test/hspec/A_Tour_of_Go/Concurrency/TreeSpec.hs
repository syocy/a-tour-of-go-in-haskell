module A_Tour_of_Go.Concurrency.TreeSpec where

import Test.Hspec
import A_Tour_of_Go.Concurrency.Tree

spec :: Spec
spec = do
  it "shuffle" $ do
    let abc = "abc"
    shuffle [1,2,3] abc `shouldBe` "abc"
    shuffle [3,2,1] abc `shouldBe` "cba"
    shuffle [2,1,3] abc `shouldBe` "bac"
  -- it "s" $ do
  --   print =<< newTree 1
  --   print =<< newTree 2
  --   print =<< newTree 3
  --   'a' `shouldBe` 'b'
