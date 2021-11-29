--- 4.6
import Debug.Trace

factorial':: [Integer]
factorial' = 1 : 1 : zipWith (*) [2..] (tail factorial')

factorial :: Integer -> Integer
factorial n = factorial' !! max 0 (fromIntegral n)

binom :: Integer -> Integer -> Integer
binom n k
    | k < n - k = product [n-k+1..n] `div` factorial k
    | otherwise = product [k+1..n] `div` factorial (n-k)

pascal :: [[Integer]]
pascal = [[binom n k | k <- [0..n]] | n <- [0..]]

pascal' :: [[Integer]]
pascal' = [[if (k == n) || (k == 0) then 1 else if n > k then pascal'!!(n-1)!!(k-1) + pascal'!!(n-1)!!k else binom (toInteger n) (toInteger k) | k <- [0..n]] | n <- [0..]]
