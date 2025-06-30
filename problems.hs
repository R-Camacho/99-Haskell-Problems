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

