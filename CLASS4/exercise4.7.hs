--- 4.7
data Arv a = Vazia | No a (Arv a) (Arv a)
    deriving (Show)

sumArv :: Num a => Arv a -> a
sumArv Vazia = 0
sumArv (No a aLeft aRight) = a + sumArv aLeft + sumArv aRight

--- 4.8

listar :: Arv a -> [a]
listar Vazia = []
listar (No a aLeft aRight) = listar aRight ++ [a] ++ listar aLeft

--- 4.9

nivel :: Int -> Arv a -> [a]
nivel _ Vazia = []
nivel 0 (No a _ _) = [a]
nivel n (No a aLeft aRight) = nivel (n-1) aLeft ++ nivel (n-1) aRight

--- 4.10

mapArv :: (a -> b) -> Arv a -> Arv b
mapArv function Vazia = Vazia
mapArv function (No a aLeft aRight) = No (function a) (mapArv function aLeft) (mapArv function aRight)

--- 4.11

inserir :: Ord a => Arv a -> a -> Arv a
inserir Vazia aNew = No aNew Vazia Vazia
inserir (No aDest destLeft destRight) aNew
    | aDest < aNew = No aDest destLeft (inserir destRight aNew)
    | aDest > aNew = No aDest (inserir destLeft aNew) destRight
    | otherwise = No aDest destLeft destRight

construirSimples :: Ord a => [a] -> Arv a
construirSimples = foldr (flip inserir) Vazia

construir :: [a] -> Arv a
construir [] = Vazia
construir list = No x (construir leftArv) (construir rightArv)
    where n = div (length list) 2
          leftArv = take n list
          x:rightArv = drop n list

altura :: Arv a -> Int
altura Vazia = -1
altura (No a aLeft aRight)
    | alturaLeft > alturaRight = alturaLeft
    | otherwise = alturaRight
    where alturaLeft = 1 + altura aLeft
          alturaRight = 1 + altura aRight

--- 4.12

maisDir :: Arv a -> a
maisDir (No x _ Vazia) = x
maisDir (No x _ vLeft) = maisDir vLeft

remover :: Ord a => a -> Arv a -> Arv a
remover val (No a arvLeft Vazia)
    | val == a = arvLeft
remover val (No a Vazia arvRight)
    | val == a = arvRight
remover val (No a arvLeft arvRight)
    | val < a = No a (remover val arvLeft) arvRight
    | val > a = No a arvLeft (remover val arvRight)
    | otherwise = No z (remover z arvLeft)  arvRight
                  where z = maisDir arvLeft



-- (No 5 (No 2 (No 1 Vazia Vazia) (No 3 Vazia Vazia)) (No 7 (No 6 Vazia Vazia) (No 8 Vazia Vazia)))
-- (No 4 (No 3 Vazia Vazia) (No 5 Vazia (No 6 Vazia Vazia)))