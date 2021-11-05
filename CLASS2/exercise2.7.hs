--- 2.7
divprop :: Integer -> [Integer]
divprop n = [x | x <- [1..n], n `mod` x == 0]

primo :: Integer -> Bool
primo n = length (divprop n) == 2