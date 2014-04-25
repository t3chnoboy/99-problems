module NinetyNineProblems.TwentyOneToTwentyEightSpec (main, spec) where

import Test.Hspec
import NinetyNineProblems.TwentyOneToTwentyEight

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "insertAt" $ do
      it "should insert an element at a given position into a list" $ do
        insertAt 'X' "abcd" 2 `shouldBe` "aXbcd"

    describe "range" $ do
      it "should create a list containing all integers within a given range" $ do
        range 4 9 `shouldBe` [4,5,6,7,8,9]

    -- describe "rnd_select" $ do
    --   it "should extract a given number of randomly selected elements from a list." $ do
    --     rnd_select "abcdefgh" 3
