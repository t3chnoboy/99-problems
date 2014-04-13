module NinetyNineProblems.ElevenToTwenty where

import NinetyNineProblems.OneToTen

-- 14.)  Duplicate the elements of a list.
dupli :: [b] -> [b]
dupli = concatMap $ replicate 2
