--- 4.1
primos :: [Int]
primos = crivo [2..]

crivo :: [Int] -> [Int]
crivo (p:xs) = p : crivo [x | x <- xs, x `mod` p /= 0] 

fatores :: Int -> [Int]
fatores 1 = []
fatores x = h : fatores (x `div` h)
    where h = head (dropWhile (\k -> x `mod` k /= 0) primos)