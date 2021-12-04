import Data.ByteString (intersperse)
import Control.Arrow (ArrowChoice(right))
--- 5.4
data Set a = Node a (Set a) (Set a) |
             Empty
    deriving (Show)

empty :: Set a
empty = Empty

member :: Ord a => a -> Set a -> Bool
member _ Empty = False
member val (Node nodeVal leftNode rightNode)
    | val < nodeVal = member val leftNode
    | val > nodeVal = member val rightNode
    | otherwise = True

insert :: Ord a => a -> Set a -> Set a
insert val Empty = Node val Empty Empty
insert val (Node nodeVal leftNode rightNode)
    | val < nodeVal = Node nodeVal (insert val leftNode) rightNode
    | val > nodeVal = Node nodeVal leftNode (insert val rightNode)
    | otherwise = Node nodeVal leftNode rightNode

getAll :: Set a -> [a]
getAll Empty = []
getAll (Node val left right) = getAll left ++ [val] ++ getAll right

union :: Ord a => Set a -> Set a -> Set a
union Empty setRight = setRight
union (Node val leftNode rightNode) setRight =  union rightNode $ union leftNode (insert val setRight)

intersect :: Ord a => Set a -> Set a -> Set a
intersect Empty setRight = Empty
intersect (Node val leftNode rightNode) setRight
    | member val setRight = insert val res
    | otherwise = res
        where res = intersect leftNode setRight `union` intersect rightNode setRight

difference :: Ord a => Set a -> Set a -> Set a
difference Empty setRight = Empty
difference (Node val leftNode rightNode) setRight
    | member val setRight = res
    | otherwise = insert val res
        where res = difference leftNode setRight `union` difference rightNode setRight

length :: Foldable t => t b -> Int
length = foldr (\_ n -> n+1) 0

-- let st2 = insert 3 $ insert 6 $ insert 4 $ insert 11 $ insert 7 Empty
-- let st1 = insert 11 $ insert 6 $insert 3 $ insert 15 $ insert 5 $ insert 10 Empty