---- 5.3

data Conjunto a = Conjunto [a]
    deriving (Show)

empty :: Conjunto a
empty = Conjunto []

member :: Eq a => a -> Conjunto a -> Bool
member val (Conjunto []) = False
member val (Conjunto (t:arr))
    | val == t = True
    | otherwise = member val (Conjunto arr)

insert :: Eq a =>a -> Conjunto a -> Conjunto a
insert val (Conjunto arr)
    | member val (Conjunto arr) = Conjunto arr
    | otherwise = Conjunto (val:arr)
