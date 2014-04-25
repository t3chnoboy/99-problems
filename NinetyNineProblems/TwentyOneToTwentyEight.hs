module NinetyNineProblems.TwentyOneToTwentyEight where


-- 21.) Insert an element at a given position into a list.
insertAt :: a -> [a] -> Int -> [a]
insertAt el list index = take (index - 1) list ++ [el] ++ drop (index - 1) list


-- 22.) Create a list containing all integers within a given range.
range :: Enum t => t -> t -> [t]
range a b = [x| x <- [a..b]]
