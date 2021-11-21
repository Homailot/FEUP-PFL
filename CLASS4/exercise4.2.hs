--- 4.2
calcPi1 :: Int -> Double
calcPi1 n = sum (take n (zipWith (/) (cycle [4,-4]) [1,3..]))

aux :: Double -> Double
aux n = n * (n+1) * (n+2)

calcPi2 :: Int -> Double
calcPi2 n = sum (take n (3 : zipWith(/) (cycle [4,-4]) [aux 2, aux 4..]))

