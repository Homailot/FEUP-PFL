--- 2.4
divprop :: Integer -> [Integer]
divprop n = [x | x <- [1..n], n `mod` x == 0]