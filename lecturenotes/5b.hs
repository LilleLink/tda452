--------------------------------------------------------------------------------
-- The monad typeclass --
-- monadic value: expression which type is of class monad
-- instructions sometimes equal actions

-- To be in the monad class, define
    -- >>= (bind): takes an m a, and instructions to produce an m b of an a, and returns the m b
    -- return : a -> m a

-- Nowadays monad is a subclass of Applicative, and then Functors

-- The truth aboud do notation
-- Its just shorthand for standard monad functions bind in combination with lambda.
-- do act1; act2 == act1 >>= \_ -> act2
-- do v <- act1; act2 == act1 >>= \v -> act2

take10 = do
    filename <- getLine
    contents <- readFile filename
    putStr (take 10 contents)

take10'  = getLine >>= \filename -> readFile filename >>= \contents -> putStr (take 10 contents)
take10'' = getLine >>= readFile >>= putStr . take 10
