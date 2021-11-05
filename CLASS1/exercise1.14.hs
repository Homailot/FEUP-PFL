--- 14
curta :: [a] -> Bool
curta lst = length lst <=2

curta2 :: [a] -> Bool
curta2 [] = True
curta2 [_] = True
curta2 [_, _] = True
curta2 (_:_:_:_) = False