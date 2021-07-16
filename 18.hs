slice :: [a] -> Int -> Int -> [a]
slice list i j | i > 0 =  take (j-i+1) $ drop (i-1) list
