--- 4.5
stringToNum :: String -> [Int]
stringToNum = map (\x -> fromEnum x - fromEnum 'A')

cifraChave :: String -> String -> String
cifraChave secret password = map (\x -> toEnum (x + fromEnum 'A') :: Char) (zipWith (\x y -> (x + y) `mod` (fromEnum 'Z' - fromEnum 'A' + 1)) (stringToNum secret) (cycle (stringToNum password)))
