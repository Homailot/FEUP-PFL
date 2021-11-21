--- 3.7
(++-) :: [a] -> [a] -> [a]
(++-) left right = foldr (:) right left

concat' :: [[a]] -> [a]
concat' = foldr (++) []

reverse' :: [a] -> [a]
reverse' = foldr (\x list -> list ++ [x]) []

reverse'' :: [a] -> [a]
reverse'' = foldl (\list x -> x : list) []

elem' :: Eq a => a -> [a] -> Bool
elem' val = any (==val)