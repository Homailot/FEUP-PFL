--- 2.16
algarismosRev :: Int -> [Int]
algarismosRev n
    | n > 0 = n `mod` 10 : algarismosRev (n `div` 10)
    | otherwise = []

algarismosRev' :: Int -> [Int]
algarismosRev' n = [k `mod` 10 | k <- takeWhile (>0) (iterate (`div` 10) n)]

algarismos :: Int -> [Int]
algarismos 0 = [0]
algarismos n = reverse (algarismosRev' n)
