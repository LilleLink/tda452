-- Basic input and output
-- Haskell is free of side effects
-- Purely functional - haskell functions are real functions.
-- How do we keep pure and still interact with IO.

------------------------------------------------------------
-- Small examples of IO
-- writeFile = writes a given value in a file that we want.
-- Type of this function Filepath -> String -> IO ()
-- The filepath is a synonym for a string. ("type" keyword)
-- The "IO ()" type is the type of instructions which interact
-- with the OS.

------------------------------------------------------------
-- IO with results - the do notation
-- readFile results in IO String, it has a result.
-- But it's not a string, IO String is part of more complex
-- instructions which can compute a string.
-- So how do we extract the string? We cannot get a string from
-- IO string, if we could it would break the purity.
-- We can still do it right though, but we need to code some to
-- show how.

-- For example:
-- We cannot do writeFile ex1 (readFile ex2) because the return
-- type of readFile is IO string, not string.
-- We can define copyFile to get around this

copyFile :: FilePath -> FilePath -> IO()
copyFile fromFile toFile = do 
    c <- readFile fromFile
    writeFile toFile c

-- var :: a, exrp :: IO a
-- var <- expr
-- do is going to "build" something of type IO
-- Its some special syntax to actually get a string from an IO string
-- We can only do this WITHIN the do block. 

------------------------------------------------------------
-- What if we want to find the longest work in a given file?:
longest :: IO String
longest = do
    wList <- readFile "usr/share/dict/words"
    return (long wList)
    where long :: String -> String
          long = snd . maximum . map (\w -> (length w, w)) . words


------------------------------------------------------------
-- IO as a first class type-
-- Types of IO are first class citizens

dotwice :: IO a -> IO (a,a)
dotwice i = do 
    a1 <- i
    a2 <- i
    return (a1,a2)

-- We are sending an instruction as a parameter
-- a1 is a something <- i is an IO of a something

-- Below does nothing
dont :: IO a -> IO ()
dont i = return ()
-- There is a difference between running an instruction and taking
-- one as a parameter. It's not run until it meets the OS in, for ex
-- a do block.

-- Return is not a "control flow" instruction, it does not behave
-- like in java. Two consecutive returns will both run:
test :: IO Int 
test = do 
    return 0
    return 42
-- Will return 42.

------------------------------------------------------------
-- Control operators - sequence

-- sequence_ - takes a list of instructions and runs them
mySequence_ :: [IO a] -> IO()
mySequence_ []     = return ()
mySequence_ (i:is) = do 
    i
    mySequence_ is

-- sequence
mySequence :: [IO a] -> IO [a]
mySequence [] = return []
mySequence (i:is) = do
    a <- i
    as <- mySequence is
    return (a:as)