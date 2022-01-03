{--

-- Question 1 --
a: 
chat :: Int -> (a -> a) -> [a] -> [a]

b:
chat' :: Int -> (a -> a) -> [a] -> [a]
chat' i f as = take i as ++ [f (as !! (i+1))] ++ drop (i+1) as

-- Below is safer since !! can crash
chat'' n f xs = case drop n xs of
                        [] -> xs
                        (y:ys) -> take n xs ++ [f y] ++ ys

c:
prop_chatEquality :: Int -> [a] -> Bool
prop_chatEquality n xs = chat n' (+1) xs == chat' n' (+1) xs 
    where
        n' = abs n

d:
findIn :: Eq a => [a] -> [a] -> Maybe Int






--}