--- 2.9

intToChar :: Int -> Char
intToChar i = toEnum i :: Char 

addLetter :: Int -> Char -> Char
addLetter n letter
    | letter >= 'a' && letter <= 'z' = if k > z then intToChar (until (z>) ((-diff)+) k) else if k < a then intToChar (until (a<) (diff+) k) else intToChar k
    | letter >= 'A' && letter <= 'Z' = if k > cz then intToChar (until (cz>) ((-diff)+) k) else if k < ca then intToChar (until (ca<) (diff+) k) else intToChar k
    | otherwise = letter
    where k = fromEnum letter + n
          z = fromEnum 'z'
          a = fromEnum 'a'
          cz = fromEnum 'Z'
          ca = fromEnum 'A'
          diff = z - a + 1

cifrar :: Int -> String -> String
cifrar n message = [addLetter n letter | letter<-message]