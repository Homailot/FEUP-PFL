--- 12
xor :: Bool -> Bool -> Bool
True `xor` True = False
True `xor` False = True
False `xor` True = True
False  `xor` False = False

xor2 :: Bool -> Bool -> Bool
True `xor2` x = not x
False `xor2` x = x

