-- This module is a starting point for implementing the Graph Drawing
-- Calculator as described in Part II of the Standard Lab. You can use this
-- directly, or just study it as an example of how to use threepenny-gui.

import ThreepennyPages
import Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny as UI
import Expr

canWidth,canHeight :: Num a => a
canWidth  = 300
canHeight = 300

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window =
  do -- Create them user interface elements
     canvas  <- mkCanvas canWidth canHeight   -- The drawing area
     fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
     input   <- mkInput 20 "x"                -- The formula input
     draw    <- mkButton "Draw graph"         -- The draw button
     slider  <- mkSlider (1,100) 1            -- The slider
     diff    <- mkButton "Differentiate"      -- The differentiate button
    -- The markup "<i>...</i>" means that the text inside should be rendered
    -- in italics.

     -- Add the user interface elements to the page, creating a 
     -- specific layout
     formula <- row [pure fx,pure input, pure slider]
     getBody window #+ [column [pure canvas,pure formula,pure draw, 
                        pure diff]]

     -- Styling
     getBody window # set style [("backgroundColor","lightblue"),
                                 ("textAlign","center")]
     pure input # set style [("fontSize","14pt")]

     -- Interaction (install event handlers)
     on UI.click      draw   $ \ _ -> readAndDraw input canvas slider
     on valueChange'  input  $ \ _ -> readAndDraw input canvas slider
     on valueChange'  slider $ \ _ -> readAndDraw input canvas slider
     on UI.click      diff   $ \ _ -> do 
                                currentValue <- get value input
                                case readExpr currentValue of
                                    Just expr -> set value 
                                                (show (differentiate expr))
                                                (pure input)
                                    Nothing   -> set value 
                                                currentValue (pure input)
     

     on UI.click      diff   $ \ _ -> readAndDraw input canvas slider
                        
--H
-- | Maps a function onto a range of x-values and returns the values
-- as points, where each point represents the position of a pixel.
points :: Expr -> Double -> (Int,Int) -> [Point]
points expr scale (w,h) = zip xs $ map 
    ((fromIntegral . round . realToPix . (expr `eval`)) . pixToReal) xs
    where
        xs = map fromIntegral [0..w]
        -- converts a pixel x-coordinate to a real x-coordinate
        pixToReal :: Double -> Double
        pixToReal x = scale * (x - fromIntegral (w `div` 2))

        -- converts a real y-coordinate to a pixel y-coordinate
        realToPix :: Double -> Double
        realToPix y = (-y + (fromIntegral h * scale / 2)) / scale

--I
-- | Reads an expression from the input text element, evaluates it,
-- then draws it on the canvas.
readAndDraw :: Element -> Canvas -> Element -> UI ()
readAndDraw input canvas slider = do
    formula <- get value input
    zoom    <- (read :: String -> Double) <$> get value slider 
    let scale = 0.04 - (zoom*0.0003) -- Calculates the scale with respect
    clearCanvas canvas               -- to the zoom value from the slider.
    path "gray" [(canWidth/2,0),(canWidth/2,canHeight)] canvas
    path "gray" [(0,canHeight/2),(canWidth, canHeight/2)] canvas
    case readExpr formula of
        Just expr -> do
            UI.fillText (show expr) (10,10) canvas
            path "blue" (points expr scale (canWidth, canHeight)) canvas
        Nothing     -> UI.fillText "Error parsing expression" (10,10) canvas

--J
-- Implemented in setup and readAndDraw

