module OneToTen where


-- 1.) Find the last element of a list.
myLast :: [c] -> c
myLast = head . reverse

-- 2.) Find the last but one element of a list.
myButLast :: [a] -> a
myButLast list = reverse list !! 2

-- 3.) Find the K'th element of a list. The first element in the list is
elementAt :: [a] -> Int -> a
elementAt list k = list !! (k - 1)

-- 4.) Find the number of elements of a list.
myLength :: [b] -> Integer
myLength = foldl (\acc _ -> acc + 1) 0

-- 5.) Reverse a list.
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- 6.) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (x:xs)|x /= last xs = False
                   |otherwise = isPalindrome $ init xs

-- 7.) Flatten a nested list structure.
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList t -> [t]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x

-- 8.) Eliminate consecutive duplicates of list elements.
compress :: Eq a => [a] -> [a]
compress xs = foldr (\x acc -> if x == head acc then acc else x:acc) [last xs] xs

-- 9.) Pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements they should be placed in separate sublists.
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack ys@(x:_) = takeWhile (==x) ys : pack (dropWhile (==x) ys)

-- 10.) Run-length encoding of a list. Use the result of problem P09 to
-- implement the so-called run-length encoding data compression method.
-- Consecutive duplicates of elements are encoded as lists (N E) where N is
-- the number of duplicates of the element E.
encode :: Eq t => [t] -> [(Int, t)]
encode = map (\x -> (length x, head x)) . pack
