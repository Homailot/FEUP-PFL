--- 2.8
-- ver 1
factorial :: Integer -> Integer
-- factorial n
--     | n > 0 =  n * factorial(n-1)
--     | otherwise = 1

-- ver 2
-- factorial n = product [n, n-1..2]

-- ver 3
--factorial' (first:second:xs) = first : factorial' ((first * second) : xs)
--factorial n  = (1 : factorial' [1..]) !! max 0 (fromIntegral n)

-- ver 4 from stack overflow adaptado
--factorial' = 1 : 1 :  [first * second | (first, second)<-zip [2..] (tail factorial')]
--factorial n = factorial' !! max 0 (fromIntegral n)

-- ver 5 from stack overflow original
factorial' = 1 : 1 : zipWith (*) [2..] (tail factorial')
factorial n = factorial' !! max 0 (fromIntegral n)

binom :: Integer -> Integer -> Integer
binom n k
    | k < n - k = product [n-k+1..n] `div` factorial k
    | otherwise = product [k+1..n] `div` factorial (n-k)


pascal :: Integer -> [[Integer]]
pascal n = [[binom line k | k <- [0..line]] | line<-[0..n]]