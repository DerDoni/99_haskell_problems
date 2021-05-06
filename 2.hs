-- Find the last but one element of a list

myButLast :: [a] -> a
myButLast [] = error "Empty list or one element list are not accepted!"
myButLast [x,y] = x
myButLast (x:xs) = myButLast xs

myButLast' :: [a] -> a
myButLast' = head . tail . reverse
