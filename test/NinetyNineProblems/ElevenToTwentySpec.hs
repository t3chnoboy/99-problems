module NinetyNineProblems.ElevenToTwentySpec (main, spec) where

import Test.Hspec
import NinetyNineProblems.ElevenToTwenty
import NinetyNineProblems.OneToTen

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "dupli" $ do
      it "should duplicate the elements of a list" $ do
        dupli [1, 2, 3] `shouldBe` [1, 1, 2, 2, 3, 3]

    describe "repli" $ do
      it "should replicate the elements of a list a given number of times" $ do
        repli "abc" 3 `shouldBe` "aaabbbccc"

    describe "dropEvery" $ do
      it "should drop every N'th element from a list" $ do
        dropEvery "abcdefghik" 3 `shouldBe` "abdeghk"
