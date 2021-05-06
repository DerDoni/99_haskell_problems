-- Find the k'th element of a list, first element in the list is number 1

elementAt :: [a] -> Int -> a
elementAt [] _ = error "empty lists are not allowed!"
elementAt (x:xs) i 
  | i > 0 = (x:xs) !! (i-1)
  | otherwise = error "index can only be positive!"
