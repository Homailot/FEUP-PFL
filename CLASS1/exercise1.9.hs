--- 1.9
classifica :: Int -> String
classifica nota = if nota <= 9 then "reprovado" else (
                    if nota <= 12 then "suficiente" else (
                        if nota <= 15 then "bom" else (
                            if nota <= 18 then "muito bom" else (
                                if nota <= 20 then "muito bom com distinção" else ""
                            )
                        )
                  )
                )           


classifica2 :: Int -> String
classifica2 nota
    | nota <= 9 = "reprovado"
    | nota <= 12 = "suficiente"
    | nota <= 15 = "bom"
    | nota <= 18 = "muito bom"
    | nota <= 20 = "muito bom com distincao"
    | otherwise = ""