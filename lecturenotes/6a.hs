module Main where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import System.Random

import Data.List
import Data.Char
import Data.IORef

data Hangman = HM { word :: String
                  , guesses :: [Char]
                  , guessesLeft :: Int}
  deriving (Show)

renderHangman :: Hangman -> String
renderHangman (HM w g gl) =
             unlines [intersperse ' ' $
                       map (\c -> if c `elem` g then c else '_') w
                     , "Guessed: " ++ (nub g)
                     , "Guesses left: " ++ show gl ]


won :: Hangman -> Bool
won hm = all (\c -> elem c (guesses hm)) (word hm)

lost :: Hangman -> Bool
lost hm = (guessesLeft hm) <= 0

updateHangman :: Char -> Hangman -> Hangman
updateHangman c hm = hm { guesses = c':(guesses hm)
                        , guessesLeft = if c' `elem` (word hm)
                                        || c' `elem` (guesses hm)
                                        then guessesLeft hm
                                        else guessesLeft hm-1 }
  where c' = toLower c

newHangman :: IO Hangman
newHangman = do fileContents <- readFile "words"
                let words = lines fileContents
                g <- newStdGen
                let (i, _) = randomR (0, length words) g
                    word = map toLower $ words !! i
                return (HM {word = word, guesses = [], guessesLeft = 6})

-- Point = (Double, Double)
type Line = (UI.Point, UI.Point)


drawLine :: UI.Canvas -> Line -> UI ()
drawLine canvas (start, end) =
     do UI.beginPath canvas
        UI.moveTo start canvas
        UI.lineTo end canvas
        UI.closePath canvas
        UI.stroke canvas

type Circle = (UI.Point, UI.Point)

drawCircle :: UI.Canvas -> Circle -> UI ()
drawCircle canvas (start, end) =
     do UI.beginPath canvas
        UI.arc start radius 0 (2*3.1415926535) canvas
        UI.closePath canvas
        UI.stroke canvas

  where radius = sqrt((sx-ex)^2 + (sy -ey)^2)
        (sx, sy) = start
        (ex, ey) = end

parts :: [([Line],[Circle])]
parts = [ ([ ((139.0,46.0),(139.0,70.0)),((65.0,44.0),(66.0,185.0))
        , ((165.0,44.0),(66.0,44.0)),((166.0,20.0),(165.0,44.0))
        , ((37.0,19.0),(166.0,20.0)),((36.0,186.0),(37.0,19.0))
        , ((65.0,186.0),(36.0,186.0)) ], []) -- Gallows
        , ([], [((140.0,89.0),(140.0,71.0))]) -- Head
        , ([((140.0,134.0),(140.0,110.0))],[]) -- Body
        , ([((131.0,147.0),(140.0,134.0))],[]) -- Left leg
        , ([((150.0,147.0),(142.0,136.0))],[]) -- Right leg
        , ([((130.0,128.0),(140.0,116.0))],[]) -- Left arm
        , ([((152.0,126.0),(142.0,118.0))],[]) -- Right arm
        ]

drawPart :: UI.Canvas -> ([Line], [Circle]) -> UI ()
drawPart canvas (lines, circles) =
     do mapM_ (drawLine canvas) lines
        mapM_ (drawCircle canvas) circles

setup :: Window -> UI ()
setup window = do
     return window # set UI.title "Hangman"
     button <- UI.button # set UI.text "Guess!"
     hangmanState <- liftIO newHangman
     textarea <- UI.p # set UI.text (renderHangman hangmanState)
                      # set UI.style [("white-space", "pre-line")]
     input <- UI.input # set UI.maxlength 1
     canvas <- UI.canvas # set UI.height 200
                         # set UI.width 200
                         # set UI.style [("background", "white")
                                        , ("border", "1px solid")]
     getBody window #+ [ element canvas
                       , element textarea
                       , element input
                       , element button]
     hmStateRef <- liftIO $ newIORef hangmanState
     drawPart canvas (head parts)

     on UI.mousedown canvas $ \coord ->
         liftIO $ print coord

     on UI.click button $ \event ->
       do liftIO $ print "Guess made!"
          inputVal <- get value input
          liftIO $ print ("Value was " ++ inputVal)
          case inputVal of
              [c] -> do liftIO $ modifyIORef hmStateRef (updateHangman c)

              _ -> return ()
          curState <- liftIO $ readIORef hmStateRef
          let winOrLose which =
                do element textarea # set UI.text ("You "++which++"! "
                                                   ++ "The word was "
                                                   ++ (word curState))

                   -- on UI.click button $ \_ ->
                   --  runFunction (ffi "location.reload()")
                   element button # set UI.text "Play again?"


          if won curState
          then winOrLose "won"
          else if lost curState
               then do mapM_ (drawPart canvas) parts
                       winOrLose "lost"
               else do UI.clearCanvas canvas
                       let howMany = 7 - (guessesLeft curState)
                       mapM_ (drawPart canvas) (take howMany parts)
                       element textarea # set UI.text (renderHangman curState)

     return ()

main :: IO ()
main = startGUI config setup

config = defaultConfig { jsLog= \ _ -> return ()}