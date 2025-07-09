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

-- Problem 11: Modified run-length encoding.
data EncodedItem a = Single a | Multiple Int a
    deriving Show
encodeModified :: (Eq a) => [a] -> [EncodedItem a]
encodeModified xs = map (\x -> if length x == 1 then Single $ head x else Multiple (length x) (head x)) $ pack xs

-- Problem 12: Decode a run-length encoded list.Solutions
-- Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.
decodeModified :: [EncodedItem a] -> [a]
decodeModified xs = concat $ map (\x -> replicate (getNumber x) (getElement x) ) xs
    where 
        getNumber (Single x)      = 1
        getNumber (Multiple n _)  = n

        getElement (Single x)     = x
        getElement (Multiple _ x) = x

-- Problem 13: Run-length encoding of a list (direct solution).
-- (I think it's the same as problem 11)
-- TODO: review later
encodeDirect :: (Eq a) => [a] -> [EncodedItem a]
encodeDirect xs = encodeModified xs

-- Problem 14: Duplicate the elements of a list.
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x : x : dupli xs

-- Problem 15: Replicate the elements of a list a given number of times.
repli :: [a] -> Int -> [a]
repli _ 0      = []
repli [] _     = []
repli (x:xs) n = repliElem x n ++ repli xs n
    where 
        repliElem _ 0 = []
        repliElem x n = x : repliElem x (n - 1)

-- Problem 16: Drop every N'th element from a list.
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = dropHelper xs n 
    where 
        dropHelper [] _   = []
        dropHelper (x:xs) 1 = dropHelper xs n
        dropHelper (x:xs) m = x : dropHelper xs (m - 1) 

-- Problem 17: Split a list into two parts; the length of the first part is given.
split :: [a] -> Int -> ([a], [a])
split xs n     = (take n xs, drop n xs) 

-- Problem 18: Extract a slice from a list.
slice :: [a] -> Int -> Int -> [a]
slice xs n m 
    n > k     = []
    otherwise = take (m - n + 1) $ drop (n - 1) xs 
