--- 3.1

comprehension :: [a] -> (a -> a) -> (a -> Bool) -> [a]
comprehension list func predicate = [func x | x <- list, predicate x]

comprehension' :: [a] -> (a -> a) -> (a -> Bool) -> [a]
comprehension' list func predicate = map func (filter predicate list)
