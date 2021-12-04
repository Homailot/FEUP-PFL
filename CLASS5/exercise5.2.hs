--- 5.2

import Stack
import Text.Read

calc :: Stack Float -> String -> Stack Float
calc stack op
    | op == "+" = let val = (+) (top (pop stack)) (top stack) in push val (pop (pop stack))
    | op == "-" = let val = (-) (top (pop stack)) (top stack) in push val (pop (pop stack))
    | op == "*" = let val = (*) (top (pop stack)) (top stack) in push val (pop (pop stack))
    | op == "/" = let val = (/) (top (pop stack)) (top stack) in push val (pop (pop stack))
    | otherwise = push (read op) stack

calcular :: String -> Float
calcular expr = top  $ snd (until (\(strings, stack) -> null strings) (\(strings, stack) -> (tail strings,calc stack (head strings)) ) (words expr, empty))

main :: IO ()
main = getLine >>= (print . calcular)
