--- 2.23
addPoly :: [Int] -> [Int] -> [Int]
addPoly polyLeft polyRight
    | sizeLeft > sizeRight = zipWith (+) polyLeft (fillPoly polyRight sizeLeft)
    | sizeLeft < sizeRight = zipWith (+) (fillPoly polyLeft sizeRight) polyRight
    | otherwise = zipWith (+) polyLeft polyRight
    where sizeLeft = length polyLeft
          sizeRight = length polyRight

-- addPoly :: [Int] -> [Int] -> [Int] --- doesnt work if some sums give 0
-- addPoly polyLeft polyRight = reverse (takeWhile (>0) (zipWith (+) (reverse polyLeft ++ repeat 0) (reverse polyRight ++ repeat 0)))


fillPoly :: [Int] -> Int -> [Int]
fillPoly poly size = replicate (size - length poly) 0 ++ poly

fillPolyFront :: Int -> Int -> [Int]
fillPolyFront number order = number : replicate order 0

-- fillPoly :: Int -> Int -> Int-> [Int]
-- fillPoly number size order = replicate (size - order - 1) 0 ++ [number] ++ replicate order 0

multPoly :: [Int] -> [Int] -> [Int]
multPoly polyLeft polyRight =foldr addPoly [] [fillPolyFront (first * second) (firstI + secondI) | (first,firstI) <- zip polyLeft [sizeLeft, sizeLeft-1..0] , (second, secondI) <- zip polyRight [sizeRight, sizeRight - 1..0]]
    where sizeLeft = length polyLeft - 1
          sizeRight = length polyRight - 1
