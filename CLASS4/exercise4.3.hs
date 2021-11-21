--- 4.3
intercalar :: a -> [a] -> [[a]]
intercalar n [] = [[n]] 
intercalar n list = (n : list) : map (head list:)  (intercalar n (tail list))
