--- 2.14
nub :: Eq a => [a] -> [a]
nub (x:xs) = x : nub elim
    where elim = [k | k<-xs, k /= x]
nub [] = []

nub2 :: Eq a => [a] -> [a]
nub2 xs
    | null xs = []
    | otherwise = x : nub [k | k<-rest, k /= x]
    where x = head xs
          rest = tail xs
