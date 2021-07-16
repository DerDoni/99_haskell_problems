-- | Drop every N'th element from a list.

dropEvery :: [a]  -> Int -> [a]
dropEvery [] _ = []
dropEvery (x:xs) n = g (x:xs) n
  where
    g (x:xs) 1 = g xs n
    g (x:xs) m = x: g xs (m-1)
    g [] m = []
