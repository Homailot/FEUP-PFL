--- 3.2
joinInt :: Int -> Int -> Int
joinInt a b = a * 10 + b

dec2int :: [Int] -> Int
dec2int = foldl joinInt 0
