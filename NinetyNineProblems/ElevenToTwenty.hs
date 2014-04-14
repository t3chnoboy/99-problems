module NinetyNineProblems.ElevenToTwenty where

import NinetyNineProblems.OneToTen

-- 14.)  Duplicate the elements of a list.
dupli :: [b] -> [b]
dupli = concatMap $ replicate 2

-- 15.) Replicate the elements of a list a given number of times.
repli :: (Enum t1, Num t1) => [t] -> t1 -> [t]
repli xs n = [x | x <- xs, _ <-[1..n]]
