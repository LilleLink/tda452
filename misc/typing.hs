module Main where
import System.Random (Random (randomR), randomRIO )
import Control.Monad (replicateM)

dictFile :: [Char]
dictFile = "/usr/share/dict/words"

main :: IO ()
main = do
    dict <- filter (\w -> length w < 7) . lines <$> readFile dictFile
    indexes <- replicateM 30 (randomRIO (1,length dict::Int))
    let segment = map (dict !!) indexes
    putStrLn $ unwords segment
    