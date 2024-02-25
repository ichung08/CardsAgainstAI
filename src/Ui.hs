module Ui
  ( State
  , initialState
  , render
  , handleEvents
  , update
  ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game -- Import the Game module for handling events

-- Initial state of the application
data State = State
  { buttonClicked :: Bool -- To track whether the button has been clicked
  }

initialState :: State
initialState = State { buttonClicked = False }

-- Function to render the scene
render :: State -> Picture
render state = pictures [cards, button (buttonClicked state)]
  where
    cards = pictures [translate x 0 (card (show (n + 1))) | (x, n) <- zip [-400, -200, 0, 200, 400] [0..4]]
    button clicked = pictures [ translate 0 (-200) $ color (if clicked then red else green) $ rectangleSolid 200 100
                              , translate (-50) (-210) $ scale 0.25 0.25 $ color black $ text "Play"
                              ]

-- Function to draw a single card
card :: String -> Picture
card num = pictures [ color white (rectangleSolid 120 160)
                    , color black (rectangleWire 120 160)
                    , translate (-30) (-10) . scale 0.2 0.2 . color black . text $ num]

-- Handling events
handleEvents :: Event -> State -> State
handleEvents (EventKey (MouseButton LeftButton) Up _ mousePos) state
  | inButtonArea mousePos = state { buttonClicked = not (buttonClicked state) }
  | otherwise = state
handleEvents _ state = state -- Ignore other events

-- Function to check if the mouse is within the button area
inButtonArea :: Point -> Bool
inButtonArea (x, y) = abs x <= 100 && y <= -150 && y >= -250

-- Update the game state (not needed for this simple example, but required by `play`)
update :: Float -> State -> State
update _ state = state
