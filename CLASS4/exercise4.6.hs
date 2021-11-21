--- 4.6
factorial :: Integer -> Integer
factorial':: [Integer]
factorial' = 1 : 1 : zipWith (*) [2..] (tail factorial')
factorial n = factorial' !! max 0 (fromIntegral n)

binom :: Integer -> Integer -> Integer
binom n k
    | k < n - k = product [n-k+1..n] `div` factorial k
    | otherwise = product [k+1..n] `div` factorial (n-k)

pascal :: [[Integer]]
pascal = [[binom n k | k <- [0..n]] | n <- [0..]]

pascal' :: [[Integer]]
pascal' = [[if (k == n) then (pascal'!!n!!0) else ]]