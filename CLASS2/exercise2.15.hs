--- 2.15
intersperse :: a -> [a] -> [a]
intersperse value list = init (concat [first : [second] | (first, second) <- zip list (repeat value)])

intersperse' :: a -> [a] -> [a]
intersperse' value (x:list)
    | size > 0 =  x : value : intersperse' value list
    | otherwise = [x]
    where size = length list
intersperse' value [] = []

intersperse'' :: a -> [a] -> [a]
intersperse'' value list = concat [if index == length list then [first] else first : [value] |  (index, first) <-  zip [1..length list] list]
