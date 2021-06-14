import Data.List
data TP a = Single a | Multiple Int a deriving (Show)


encode :: Eq a => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . group

encodeModified :: Eq a => [a] -> [TP a]
encodeModified = map unpack . encode
  where
    unpack (1, x) = Single x
    unpack (n, m) = Multiple n m
