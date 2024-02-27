{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Testui where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.List (findIndex)

-- World datatype
data World = World { state :: Int, gameState :: Int, cardValue :: String }

-- Window configuration
window :: Display
window = InWindow "Button UI" (800, 600) (100, 100)

-- Initial world state with initial card value
initialUiState :: World
initialUiState = World { state = 0, gameState = 0, cardValue = "Initial" }

-- Button properties
buttonWidth = 80 :: Float
buttonHeight = 40 :: Float
startY = -290 :: Float

-- Drawing function
drawWorld :: World -> Picture
drawWorld world
  | gameState world == 0 = Pictures $ map drawButton [0..9] ++ [stateCard, dynamicCard]  -- Add dynamicCard to the Pictures list
  | gameState world == 1 = votingPhase
  | gameState world == 2 = translate (-100) 0 $ scale 0.3 0.3 $ text "You Win!"
  where
    buttonColors = [red, orange, yellow, green, blue, violet, cyan, magenta, rose, azure]
    drawButton i = let (x, _) = buttonPositions !! i
                       col = buttonColors !! i
                   in translate x startY $ color col $ rectangleSolid buttonWidth buttonHeight
    stateCard = Pictures [ translate 0 0 $ color white $ rectangleSolid 100 140,
                           translate 0 0 $ color black $ rectangleWire 100 140,
                           translate (-30) 0 $ scale 0.2 0.2 $ color black $ text $ "State: " ++ show (state world) ]
    dynamicCard = Pictures [ translate 0 200 $ color white $ rectangleSolid 100 140,  -- Position adjusted for visibility
                             translate 0 200 $ color black $ rectangleWire 100 140,
                             translate (-45) 200 $ scale 0.15 0.15 $ color black $ text $ "Card: " ++ cardValue world ]
    votingPhase = Pictures [ translate (-150) 0 $ color white $ rectangleSolid 100 140,  -- Card A
                             translate (-150) 0 $ color black $ rectangleWire 100 140,
                             translate (-180) 0 $ scale 0.2 0.2 $ color black $ text "A",
                             translate (150) 0 $ color white $ rectangleSolid 100 140,  -- Card B
                             translate (150) 0 $ color black $ rectangleWire 100 140,
                             translate (120) 0 $ scale 0.2 0.2 $ color black $ text "B" ]

-- Handling events
handleEvent :: Event -> World -> World
handleEvent (EventKey (MouseButton LeftButton) Up _ mousePos) world =
    case gameState world of
      0 -> handleDefaultPhase mousePos world
      1 -> handleVotingPhase mousePos world
      2 -> world  -- No action in the end game phase
      _ -> world  -- Catch-all for unexpected gameState values
  where
    handleDefaultPhase mousePos world =
      case buttonClicked mousePos of
        Just n  -> case n of
                     7 -> world { cardValue = "New card value"}
                     8 -> world { gameState = 1 }  -- Transition to voting
                     9 -> world { gameState = 2 }  -- End game
                     _ -> world { state = n }
        Nothing -> world

    handleVotingPhase mousePos world
      | isClickedOnCardA mousePos = world { gameState = 0, state = 10 } -- logic based on vote
      | isClickedOnCardB mousePos = world { gameState = 0, state = 11 } -- (should just increment score here)
      | otherwise = world

    isClickedOnCardA (x, y) = x >= -250 && x <= -50 && y >= -70 && y <= 70
    isClickedOnCardB (x, y) = x >= 50 && x <= 250 && y >= -70 && y <= 70

handleEvent _ world = world  -- Handle other events

-- Check if a button is clicked and return its index
buttonClicked :: Point -> Maybe Int
buttonClicked (x, y) = findIndex (isClicked x y) buttonPositions
  where
    isClicked xClick yClick (xPos, yPos) =
        xClick >= xPos - buttonWidth / 2 && xClick <= xPos + buttonWidth / 2 &&
        yClick >= yPos - buttonHeight / 2 && yClick <= yPos + buttonHeight / 2

-- Button positions
buttonPositions :: [(Float, Float)]
buttonPositions = [((-360) + buttonWidth * fromIntegral i, startY) | i <- [0..9]]

-- Update function, not used in this example
updateWorld :: Float -> World -> World
updateWorld _ = id
