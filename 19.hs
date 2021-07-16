rotate :: [a] -> Int -> [a]
rotate list n
 | n >= 0 =  drop n list ++ x
 | otherwise = y ++ take (l - abs n) list
   where
     x = take n list
     y = drop  (l + n) list
     l = length list
