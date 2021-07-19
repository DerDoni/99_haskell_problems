removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt k list
  | (k < 0) || (k > length list) = (Nothing, list)
  | otherwise = (Just (last $ take k list), take (k-1) list ++ drop k list)
