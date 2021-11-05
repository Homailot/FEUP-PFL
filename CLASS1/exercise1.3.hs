--- 1.3
metades :: [a] -> ([a], [a])
metades list = (take size list, drop size list)
  where
    size = div (length list) 2

metades2 :: [a] -> ([a], [a])
metades2 list = splitAt size list
  where
    size = div (length list) 2

-- 1.3 1.4 1.6 1.7 1.8 1.9 12 14 15 16 --