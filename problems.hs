-- Problem 1: Find the last element of a list. 
myLast :: [a] -> a
myLast [] = undefined 
myLast [x] = x
myLast (_:xs) = myLast xs
 
-- Problem 2: Find the last-but-one (or second-last) element of a list. 
myButLast :: [a] -> a
myButLast [x, _] = x
myButLast (_:xs) = myButLast xs
myButLast _ = undefined

-- Problem 3: Find the K'th element of a list.
elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt (_:xs) k = elementAt xs (k - 1)
elementAt _ _ = undefined

-- Problem 4: Find the number of elements in a list.
myLength:: [a] -> Int
myLength [] = 0
myLength [x] = 1
myLength (_:xs) = 1 + myLength xs

-- Problem 5: Reverse a list.
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- Problem 6: Find out whether a list is a palindrome.

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True 
isPalindrome (x:xs) = x == last xs && (isPalindrome $ init $ xs)

-- or using reverse, simply:
--isPalindrome xs = xs == (reverse xs)

-- Problem 7: Flatten a nested list structure.
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (Elem x) = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List xs) 

-- Problem 8: Eliminate consecutive duplicates of list elements.
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:x1:xs) 
    | x == x1   = compress $ x1 : xs
    | otherwise = x : compress (x1 : xs)

-- Problem 9: Pack consecutive duplicates of list elements into sublists.
pack :: (Eq a) => [a] -> [[a]]
pack []  = []
pack [x] = [[x]]
pack (x:xs) 
    | x `elem` (head (pack xs)) = (x : (head $ pack xs)) : (tail $ pack xs)
    | otherwise                 = [[x]] ++ pack (xs)

-- Problem 10: Run-length encoding of a list.
encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = map (\x -> (length x, head x)) $ pack xs

