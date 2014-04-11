-- 1.) Find the last element of a list.
myLast = head . reverse

-- 2.) Find the last but one element of a list.
myButLast list = (reverse list) !! 2

-- 3.) Find the K'th element of a list. The first element in the list is
elementAt list k = list !! (k - 1)

-- 4.) Find the number of elements of a list.
myLength = foldl (\acc _ -> acc + 1) 0

-- 5.) Reverse a list.
myReverse = foldl (\acc x -> x : acc) []

-- 6.) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs)|x /= last xs = False
                   |otherwise = isPalindrome(init xs)
