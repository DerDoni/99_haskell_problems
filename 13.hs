import Data.List
data TP a = Single a | Multiple Int a deriving (Show)

encode :: Eq a => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . group

encodeModified :: Eq a => [a] -> [TP a]
encodeModified = map unpack . encode
  where
    unpack (1, x) = Single x
    unpack (n, m) = Multiple n m

decodeModified :: [TP a] -> [a]
decodeModified = concatMap pack
  where
    pack (Multiple n m) = replicate n m
    pack (Single x) = [x]

encodeDirect :: Eq a => [a] -> [TP a]
encodeDirect [] = []
encodeDirect (x:xs)
  | len > 1 = (Multiple len x) : encodeDirect rest
  | otherwise = (Single x) : encodeDirect rest
  where
    (list, rest) = span (==x) xs
    len = length (x:list)
