import Control.Monad (forM_)
import System.Random( -- The actual types are more general:
  StdGen,   -- the type of standard random generators
  Random,   -- the type class for things which can be generated
  randomR,  -- :: Random a => (a, a) -> StdGen -> (a, StdGen)
  newStdGen -- :: IO StdGen 
 )
import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap)
-----------------------------------------------------------------------------
-- See MonadMaybeExample for more details --

-----------------------------------------------------------------------------
-- forloop exercise --
forLoop :: Monad m => [Int] -> (Int -> m a) -> m ()
forLoop is f = mapM_ f is

whileLoop :: Monad m => (a -> Bool) -> (Int -> m a) -> m ()
whileLoop p f = undefined -- There is a function whileM_ in the Control.Monad.Loops lib
-- Although it's not included in the prelude.

-- joinFiles n fileName creates a file 
-- by joining together fileName.part1 .. fileName.partn  
joinFiles n fileName = do
    writeFile fileName "" -- create empty file  
    forM_ [0..n] $ \i -> 
        do  c <- readFile $ fileName ++ ".part"++ show i
            appendFile fileName c
    putStrLn "Done"

-----------------------------------------------------------------------------
-- Baby Gen --
newtype Gen a = Gen (StdGen -> (a,StdGen))

runGen :: Gen a -> StdGen -> (a, StdGen)
runGen (Gen a) = a

-- Boilerplate code (we need Functor and Applicative instances,
-- but these can be defined in a standard way, since we are not 
-- discussing them in this course)
-- Imports at the top
instance Functor Gen where
  fmap = liftM
instance Applicative Gen where
  pure = return 
  (<*>) = ap
----
instance Monad Gen where
  -- a -> Gen a
  return a = Gen (\newStdGen -> (a,newStdGen))
  -- Gen a -> (a -> Gen b) -> Gen b
  (Gen a) >>= f = f (fst (a (return newStdGen)))

generate :: Gen a -> IO a
generate = undefined

choose :: Random a => (a,a) -> Gen a
choose = undefined