--- 1.6)
raizes :: Float -> Float -> Float -> (Float, Float)
raizes a b c = (((-b) + sqrt k)/(2*a), ((-b) - sqrt k)/(2*a))
            where k = (b^2) - (4 * a * c)