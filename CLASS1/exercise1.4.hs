--- 1.4
--- a)
last1 :: [a] -> a
last1 list = head (drop (length list - 1) list)

last2 :: [a] -> a
last2 list = head (take 1 (reverse list))

--- b)
init1 :: [a] -> [a]
init1 list = take (length list - 1) list

init2 :: [a] -> [a]
init2 list = reverse( drop 1 (reverse list))