import Control.Monad (forM_)
-----------------------------------------------------------------------------
-- See MonadMaybeExample for more details --

-----------------------------------------------------------------------------
-- forloop exercise --
forLoop :: Monad m => [Int] -> (Int -> m a) -> m ()
forLoop is f = mapM_ f is

-- joinFiles n fileName creates a file 
-- by joining together fileName.part1 .. fileName.partn  
joinFiles n fileName = do
    writeFile fileName "" -- create empty file  
    forM_ [0..n] $ \i -> 
        do  c <- readFile $ fileName ++ ".part"++ show i
            appendFile fileName c
    putStrLn "Done"