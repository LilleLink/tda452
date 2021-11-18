import Test.QuickCheck
import Data.List
-- (1) Write a generator that generates paris of integers where the second
-- one is at least 2 times as big as the first one
tupleGen :: Gen (Integer, Integer)
tupleGen = (\(n1, n2) -> (n1, abs n2 + double n1)) <$> arbitrary
    where double n
            | n < 0 = n `div` 2
            | otherwise = n * 2

-- (2) Write a recursive definition of vectorOf
vectorOf' :: Int -> Gen a -> Gen [a]
vectorOf' 0 _ = return []
vectorOf' n g = do
    ve <- g
    ves <- vectorOf' (n-1) g
    return (ve:ves)

-- (3) Write a function that generates a pair of strings where the second
-- string starts with the first one
prefixPair :: Gen (String, String)
prefixPair = do
    s1 <- arbitrary
    s2 <- (s1++) <$> arbitrary
    return (s1,s2)

-- (4) Write a generator which given an Int always returns a list containing
-- that Int
contains :: Int -> Gen [Int]
contains n = do
    list <- listOf arbitrary
    index <- choose (0, Prelude.length list)
    return (insertAt' index n list)

insertAt' :: Int -> Int -> [Int] -> [Int]
insertAt' i e list = insert' i e list []
    where insert' 0 e (x:xs) pre = pre ++ e:x:xs
          insert' _ e [] pre = pre ++ [e]
          insert' i e (x:xs) pre = insert' (i-1) e xs (x:pre)

-- or
contains' :: Int -> Gen [Int]
contains' n = do
    list <- listOf arbitrary
    return (insert n list)
