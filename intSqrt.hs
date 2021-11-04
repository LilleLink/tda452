import Test.QuickCheck

intSqrt :: Int -> Int
intSqrt n = aux n 0 n
    where 
    aux n lo hi
        | lo == mid = mid
        | sq > n = aux n lo mid
        | otherwise = aux n mid hi
        where
            sq = mid * mid;
            mid = (lo + hi) `div` 2

prop_intSqrt :: Int -> Bool 
prop_intSqrt n = intSqrt n' ^ 2 <= n'
    where n' = abs n