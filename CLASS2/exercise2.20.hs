--- 2.20
insert :: Ord a => a -> [a] -> [a]
insert n (x:list)
    | n > x = x : insert n list
    | otherwise = n : x : list
insert n [] = [n]

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:list) = insert x (isort list)

isort' :: Ord a => [a] -> [a]
isort' = foldr insert []
