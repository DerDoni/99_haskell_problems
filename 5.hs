-- Reverse a list

myReverse :: [a] -> [a]
myReverse [] = []
myReverse list = last list : myReverse  (init list)
