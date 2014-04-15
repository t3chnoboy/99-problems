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

    describe "split" $ do
      it "should split a list into two parts; the length of the first part is given" $ do
        split "abcdefghik" 3 `shouldBe` ("abc", "defghik")

    describe "slice" $ do
      it "should  extract a slice from a list" $ do
        slice ['a','b','c','d','e','f','g','h','i','k'] 3 7 `shouldBe` "cdefg"

    describe "rotate" $ do
      it "should rotate a list N places to the left" $ do
        rotate ['a','b','c','d','e','f','g','h'] 3 `shouldBe` "defghabc"
        rotate ['a','b','c','d','e','f','g','h'] (-2) `shouldBe` "ghabcdef"

    describe "removeAt" $ do
      it "should remove the K'th element from a list" $ do
        removeAt 2 "abcd" `shouldBe` ('b',"acd")
