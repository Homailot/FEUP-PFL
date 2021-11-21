--- 3.8
aux :: [String] -> String -> ([String], String)
aux total str = (total ++ [fst d], dropWhile (== ' ') (snd d))
    where d = span (/= ' ') str

palavras :: String -> [String]
palavras str = fst(until (\(total, rest) -> null rest) (\(total, rest) -> aux total rest) ([], str))

despalavras :: [String] -> String
despalavras = foldl ()