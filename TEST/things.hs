--- cool things

acao = do x <- getChar
          putChar x
          putChar x

test1 = getChar >> putChar '1'
