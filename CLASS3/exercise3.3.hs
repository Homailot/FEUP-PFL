--- 3.3
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' func (x:list1) (y:list2) = func x y : zipWith' func list1 list2
zipWith' func [] _ = []
zipWith' func _ [] = []