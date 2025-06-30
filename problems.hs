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
myLenght :: [a] -> Int
myLenght [] = 0
myLenght [x] = 1
myLenght (_:xs) = 1 + myLenght xs

-- Problem 5: Reverse a list.
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

