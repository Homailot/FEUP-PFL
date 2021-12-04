---- 5.1

import Stack

matchBrack :: Char -> Stack Char -> Stack Char
matchBrack c stack
    | (c == '(') || (c == '[') || (c == '{') = push c stack
    | (c == ')') && (top stack == '(') = pop stack
    | (c == ']') && (top stack == '[') = pop stack
    | (c == '}') && (top stack == '{') = pop stack
    | otherwise = stack 

parent :: String -> Bool
parent inp = isEmpty $ snd (until (\(string, stack) -> null string) (\(c:string, stack) -> (string, matchBrack c stack)) (inp, empty))
