repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = repli' n (x:xs)
  where
    repli' m (x:xs)
      | m > 0 = x : repli' (m-1) (x:xs)
      | otherwise = repli' n xs

repli'' :: [a] -> Int -> [a]
repli'' xs n = concatMap (replicate n) xs
