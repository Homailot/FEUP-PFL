--- 1.15
mediana :: Ord a => a -> a -> a -> a
mediana a b c
  | (a >= b && a <=c) || (a >=c && a<=b) = a
  | (b >= a && b <= c) || (b >= c && b <= a) = b
  | otherwise = c

mediana2 :: (Num a, Ord a) => a -> a -> a -> a
mediana2 a b c = a + b + c - menor - maior
    where menor = minimum [a,b,c]
          maior = maximum [a,b,c]