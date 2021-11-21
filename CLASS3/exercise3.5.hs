import Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable (toList)

--- 3.5
--- a)
getBiggest :: Ord a => a -> a -> a
getBiggest left right
    | left > right = left
    | otherwise = right


maximum' :: Ord a => [a] -> a
maximum' = foldl1 getBiggest

getSmallest :: Ord a => a -> a -> a
getSmallest left right
    | left < right = left
    | otherwise = right

minimum' :: Ord a => [a] -> a
minimum' = foldr1 getSmallest

--- b)
foldr1' :: Foldable t => (a -> a -> a) -> t a -> a
foldr1' func list = foldr func (last (Foldable.toList list)) (init (Foldable.toList list))

foldl1' :: Foldable t => (a -> a -> a) -> t a -> a
foldl1' func list = foldl func (head (Foldable.toList list)) (tail (Foldable.toList list))