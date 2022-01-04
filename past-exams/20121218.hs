import Data.List (intersperse)
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
findIn seq xs = findInIndex seq xs 0
    where
        findInIndex :: Eq a => [a] -> [a] -> Int -> Maybe Int
        findInIndex _   []       _ = Nothing
        findInIndex seq l@(x:xs) n | seq `isPrefixOf` l = Just n
                                   | otherwise = findInIndex seq xs (n+1)

prop_findIn0 = findIn "Hell" "Hello"      == Just 0
            && findIn "ell" "Hello Jello" == Just 1
            && findIn "Hell" "Helan"      == Nothing
        
e: 
yes

f:
prop_findIn :: Eq a => [a] -> Bool
prop_findIn ys = case xs `findIn` ys of
    Just _ -> True
    Nothing -> False
    where
        xs :: String
        xs = do
            n <- choose (0,length ys)
            fst $ splitAt n ys

-- Question 2 --
type Journey = [Leg]
type Place = String

a: 
data Leg = Leg Mode Place Place
    deriving Show

data Mode = Bus | Train | Flight
    deriving Show

b:
connected :: Journey -> Bool
connected jrny = all startDstEq (zip (init jrny) (tail jrny))

isConnected :: (Journey, Journey) -> Bool
isConnected (j1,j2) | destination j1 == start j2 = True
                    | otherwise                  = False

start :: Journey -> Place
start (Journey _ s _) = s

destination :: Journey -> Place
destination (Leg _ _ d) = d

c:
missingLegs :: Journey -> [(Place, Place)]
missingLegs (j1:[])    = []
missingLegs (j1:j2:js) | destination j1 /= start j2 = (j1,j2) : continue
                       | otherwise                  = continue
    where
        continue = missingLegs (j2:js)

prop_missingLegs j = not(null j) ==> connected j == null (missingLegs j)

d:
instance Arbitrary Leg where
    arbitrary = do
        let places = ["A", "B", "C"] -- dont like this, should be arbitary here aswell :((
        start <- elements places
        end   <- elements (delete start places)
        mode <- arbitrary
        return $ Leg mode start end 


instance Arbitrary Mode where
    arbitrary = elements [Bus, Train, Flight]

-- Question 3 --
data Map = Map PlaceName [(Dir,Map)]
    deriving Eq
data Dir = N | S | E | W 
    deriving (Eq,Show)
type PlaceName = String

-- Example:
hogwarts = Map "Castle" [(N,forest),(S,lake)]
forest = Map "Forest" [(S,hogwarts)]
lake = Map "Lake" [(N,hogwarts)]

a:
travel :: Map -> [Dir] -> Maybe Map
travel m          [] = Just m -- Found the way
travel (Map _ ms) ds = findDir ms ds 
    where
        findDir :: [(Dir, Map)] -> Maybe Map
        findDir []            ds     = Nothing -- Didn't find the way
        findDir ((dir,m):dms) (d:ds) | d == dir  = travel m ds
                                     | otherwise = findDir dms (d:ds) 

-- Better to do with lookup, didn't see the opportunity until after

b:
It would never stop printing since the map from hogwarts (forest and lake)
point back to hogwarts, this would send the print function into a infinite printing loop.

c:
instance Show Map where
    show m = "You are at the " ++ showMap m []

showMap :: Map -> [String] -> String
showMap m@(Map s ds) visited = s ++ ". " ++ showDirections ds ++ "\n" ++ showRest m
    where
        showDirections ds = concat $ intersperse ", " $ map showDir ds 
        showDir (d, m) = "Go " ++ show d ++ " to " ++ here m
        showRest (Map s _) = concat $ intersperse "\n" $ map (`showMap` (s:visited)) unvisited
        unvisited = [Map q d | (_, Map q d) <- ds, q `notElem` (s:visited)]

here (Map s _) = s

-- Question 4 --

a:
Write without do notation
backup f = do
    a <- readFile f
    let backup = f ++ ".bac"
    putStrLn $ "Creating backup in " ++ backup
    writeFile backup a

backup' :: FilePath -> IO ()
backup' f = readFile f >>= \ a ->
            let backup = f ++ ".bac" in
            putStrLn ("Creating backup in " ++ backup) >> 
            writeFile backup a

b:
for_ :: [a] -> (a -> IO()) -> IO ()
for_ as f = mapM_ f as
-- mapM_ = sequence_ . map

c:
for :: [a] -> (a -> IO b) -> IO [b]
for as f = mapM f as
-- mapM = sequence . map

d:
join :: FilePath -> Int -> IO ()
join file i = do
    let files = [file ++ ".part" ++ (show n) | n <- [1..i]]
    content <- for files (\fp -> readFile fp)
    writeFile file (concat content)


--}

