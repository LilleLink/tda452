splitUp' :: (Num a, Ord a) => [a] -> ([a],[a])
splitUp' ns = splitHelper ns [] []
    where 
        splitHelper [] pos neg = (pos,neg)
        splitHelper (x:xs) pos neg 
            | x < 0     = splitHelper xs pos (x:neg)
            | otherwise = splitHelper xs (x:pos) neg

splitUp :: (Num a, Ord a) => [a] -> ([a],[a])
splitUp []      = ([],[])
splitUp (x:xs)  | x > 0     = (x:positive, negative)
                | otherwise = (positive, x:negative)
        where (positive, negative) = splitUp xs
