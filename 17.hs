split :: [a] -> Int -> ([a], [a])
split list n = (take n list, drop n list)

split' :: [a] -> Int -> ([a], [a])
split' (x:xs) n
  | n > 0 = let (l, r) = split' xs (n-1) in (x : l, r)
split' xs _ = ([], xs)

split''' :: [a] -> Int -> ([a], [a])
split''' (x:xs) n = (x: l,r)
  where
    (l,r) = split''' xs (n-1)
split''' xs _ = ([], xs)


-- split'' (x:xs) n = helper (x:xs) n
--  where
--    helper (x:xs) a
