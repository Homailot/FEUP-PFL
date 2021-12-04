ttriangulo :: Float -> Float -> Float -> String
ttriangulo l1 l2 l3
    | l1 == l2 && l2 == l3 = "equilatero"
    | l1 == l2 || l1 == l3 || l2 == l3 = "isosceles"
    | otherwise = "escaleno"

pythagoras :: Float -> Float -> Float -> Bool
pythagoras c1 c2 h = c1*c1 + c2*c2 <= h*h + 0.001 && c1*c1 + c2*c2 >= h*h - 0.001

rectangulo :: Float -> Float -> Float -> Bool
rectangulo l1 l2 l3
    | l1 == m = pythagoras l2 l3 l1
    | l2 == m = pythagoras l3 l1 l2
    | otherwise = pythagoras l1 l2 l3
    where m = max l3 $ max l1 l2

f :: (Eq p, Num p) => p -> p
f 0 = 0
f n = n * f (n-1)

troca :: (b, a) -> (a, b)
troca (x,y) = (y,x)

g :: (Ord a, Num a) => a -> a -> a
g x y
    | x <= y = g x (y-1)
    | otherwise = x + y

maiores :: (Ord a) => [a] -> [a]
maiores [] = []
maiores [h] = []
maiores (h:lista)
    | h > head lista = (:) h $ maiores lista
    | otherwise = maiores lista

somapares :: Num a => [(a,a)] -> [a]
somapares [] = []
somapares (h:list) = [fst h + snd h] ++ somapares list


somapares' :: Num a => [(a, a)] -> [a]
somapares' list = [uncurry (+) h | h <- list]

itera :: Int -> (a -> a) -> a -> a
itera num f value = iterate f value !! num 

itera' :: Int -> (a -> a) -> a -> a
itera' 0 f value = value
itera' num f value = f $ itera' (num - 1) f value

itera'' :: (a -> a) -> a -> [a]
itera'' f value = value : itera'' f (f value)

mult :: Int -> Int -> Int
mult a b
    | b < 0 = - (itera (-b) (+a) 0)
    | otherwise = itera b (+a) 0
