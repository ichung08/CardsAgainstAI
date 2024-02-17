module Main (main) where
import Graphics.Gloss
import Lib

main :: IO ()
main = display window background drawing
  where
    window = InWindow "Cards Against AI" (1024, 768) (100, 100)
    background = white
    drawing = pictures [translate x 0 (card (show (n + 1))) | (x, n) <- zip [-400, -200, 0, 200, 400] [0..4]]

-- Function to draw a single card with a number as text
card :: String -> Picture
card num = pictures [ color white (rectangleSolid 120 160)
                    , color black (rectangleWire 120 160)
                    , translate (-30) (-10) . scale 0.2 0.2 . color black . text $ num]
