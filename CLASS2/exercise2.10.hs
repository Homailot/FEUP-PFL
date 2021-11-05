--- 2.10
myand :: [Bool] -> Bool
myand (x:xs)
    | x = myand xs
    | otherwise = False
myand [] = True

myor :: [Bool] -> Bool
myor (x:xs)
    | x = True
    | otherwise = myor xs
myor [] = False

--myconcat :: [[a]] -> [a]
--myconcat (x:xs) = x ++ myconcat xs
--myconcat [] = []

myconcat :: [[a]] -> [a]
myconcat = foldr (++) []

myreplicate :: Int -> a -> [a]
myreplicate n value 
    | n > 0 = value : myreplicate (n-1) value
    | otherwise = []

myselect :: [a] -> Int -> a
myselect xs 0 = head xs
myselect (x:xs) n = myselect xs (n-1)
myselect [] _ = error "empty list"

myelem :: Eq a => a -> [a] -> Bool
myelem n (x:xs)
    | x == n = True
    | otherwise = myelem n xs
myelem n [] = False
