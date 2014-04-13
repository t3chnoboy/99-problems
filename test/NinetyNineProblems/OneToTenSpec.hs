module NinetyNineProblems.OneToTenSpec (main, spec) where

import Test.Hspec
import NinetyNineProblems.OneToTen

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "myLast" $ do
    it "should find the last element of the list" $ do
      myLast [1, 2, 3, 4] `shouldBe` 4
      myLast ['x', 'y', 'z'] `shouldBe` 'z'

  describe "myButLast" $ do
    it "should find last but one element of a list" $ do
      myButLast [1, 2, 3, 4] `shouldBe` 3
      myButLast ['a'..'z'] `shouldBe` 'y'

  describe "elementAt" $ do
    it "should find the K'th element of a list" $ do
      elementAt [1, 2, 3] 2 `shouldBe` 2
      elementAt "haskell" 5 `shouldBe` 'e'

  describe "myLength" $ do
    it "should find the number of elements of a list" $ do
       myLength [123, 456, 789] `shouldBe` 3
       myLength "Hello, world!" `shouldBe` 13

  describe "myReverse" $ do
    it "should reverse a list" $ do
      myReverse "A man, a plan, a canal, panama!" `shouldBe` "!amanap ,lanac a ,nalp a ,nam A"
      myReverse [1, 2, 3, 4] `shouldBe` [4, 3, 2, 1]

  describe "isPalindrome" $ do
    it "should find out whether a list is a palindrome" $ do
      isPalindrome [1,2,3] `shouldBe` False
      isPalindrome "madamimadam" `shouldBe` True
      isPalindrome [1,2,4,8,16,8,4,2,1] `shouldBe` True

  describe "flatten" $ do
    it "should flatten a nested list structure" $ do
      flatten (Elem 5) `shouldBe` [5]
      flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) `shouldBe` [1, 2, 3, 4, 5]

  describe "compress" $ do
    it "should eliminate consecutive duplicates of list elements" $ do
      compress "aaaabccaadeeee" `shouldBe` "abcade"

  describe "pack" $ do
    it "should Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists." $ do
      pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'] `shouldBe` ["aaaa", "b", "cc", "aa", "d", "eeee"]

  describe "encode" $ do
    it "shoud run-length encoding of a list. Consecutive duplicates of elements are encoded as lists (N E)" $ do
      encode "aaaabccaadeeee" `shouldBe` [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
