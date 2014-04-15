module NinetyNineProblems.ElevenToTwenty where


-- 14.)  Duplicate the elements of a list.
dupli :: [b] -> [b]
dupli = concatMap $ replicate 2

-- 15.) Replicate the elements of a list a given number of times.
repli :: (Enum t1, Num t1) => [t] -> t1 -> [t]
repli xs n = [x | x <- xs, _ <-[1..n]]

-- 16.) Drop every N'th element from a list.
dropEvery :: Integral a => [a1] -> a -> [a1]
dropEvery xs n =  map fst . filter (\x -> mod (snd x) n /= 0) $ zip xs [1..]

-- 17.) Split a list into two parts; the length of the first part is given.
split :: [a] -> Int -> ([a], [a])
split xs n = (take n xs, drop n xs)

-- 18.) Extract a slice from a list
slice :: [a] -> Int -> Int -> [a]
slice xs i k = take (k - i + 1) $  drop (i - 1) xs
