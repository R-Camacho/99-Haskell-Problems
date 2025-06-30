-- Problem 1: Find the last element of a list. 
myLast :: [a] -> a
myLast [] = undefined 
myLast [x] = x
myLast (_:xs) = myLast xs
 
