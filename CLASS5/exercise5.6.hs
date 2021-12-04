import Prelude hiding (lookup)
--- 5.6

data Map k a = Node (k,a) (Map k a) (Map k a) |
               Empty
    deriving (Show)

empty :: Map k a
empty = Empty

lookup :: Ord k => k -> Map k a -> Maybe a
lookup k Empty = Nothing
lookup k (Node (key, value) mapLeft mapRight)
    | k < key = lookup k mapLeft
    | k > key = lookup k mapRight
    | otherwise = Just value

insert :: Ord k => k -> a -> Map k a -> Map k a
insert k val Empty = Node (k, val) Empty Empty
insert k val (Node (key, value) mapLeft mapRight)
    | k < key = Node (key, value) (insert k val mapLeft) mapRight
    | k > key = Node (key, value) mapLeft (insert k val mapRight)
    | otherwise = Node (key, value) mapLeft mapRight

-- let st2 = insert 3 1 $ insert 6 2 $ insert 4 3 $ insert 11 4 $ insert 7 5 Empty
-- let st1 = insert 11 $ insert 6 $insert 3 $ insert 15 $ insert 5 $ insert 10 Empty

--insert Ord k 