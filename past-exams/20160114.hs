{--

-- Question 1 --

a:
xmas :: Int -> IO()
xmas n = doprint 1 where
    doprint m = if m > n then return ()
                else do
                    printCopies (n - m) " "
                    printCopies m " *"
                    putStrLn ""
                    doprint (m + 1)

    where printCopies k s = if k <= 0 then return ()
                                else do putStr s
                                    printCopies (k-1) s

xmas' :: Int -> IO ()
xmas' n = mapM_ printTreeRow [1..n]
    where 
        printTreeRow :: Int -> IO ()
        printTreeRow m = do 
                        printCopies (n - m) " " 
                        printCopies m " *"
                        putStrLn ""

        printCopies :: Int -> String -> IO ()
        printCopies k s = replicateM_ k (putStr s)

b:
splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen f [] = [[]] -- bad - gives an extra []
splitWhen f as = fst seg : splitWhen f (drop 1 $ snd seg)
    where seg = span (not . f) as

splitWhen' :: (a -> Bool) -> [a] -> [[a]]
splitWhen' f as = case span (not . f) as of
                (first, []) -> [first]
                (first, second) -> first : splitWhen' f (drop 1 second)

prop_splitWhen0 =
    splitWhen (== ';') "A;BB;;DDDD;" == ["A","BB","","DDDD",""]
    && splitWhen (>1) [3,0,1,2,0,0] == [[],[0,1],[0,0]]
    && splitWhen (>1) [] == [[]]

c:
-- If you split in n places, we end up with n+1 segments
prop_splitWhen :: (a -> Bool) -> [a] -> Bool
prop_splitWhen f as = length (filter f as) + 1 == length (splitWhen f as)

-- Question 2 --

fa :: Eq a => a -> [a] -> [b] -> Maybe b
fa l m n = m ‘lookup‘ zip l n

fb :: [(a -> a)] -> a -> a
fb [] a = a
fb (b:c) a = fb c (b a)

fc (a:b) (c:d) = b /= c
fc _ e = null e

--}
