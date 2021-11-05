--- 2.2
aprox :: Int -> Double
aprox n = soma * 4
    where soma = sum [((-1)^x) / fromIntegral (2*x + 1)  | x <- [0..n]]

aprox' :: Int -> Double
aprox' n = sqrt (soma * 12)
    where soma = sum [((-1)^k)/fromIntegral ((k + 1)^2) | k <- [0..n]]