{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.List (findIndex)

import Cards

-- Window configuration
window :: Display
window = InWindow "Button UI" (800, 600) (100, 100)

-- Initial world state
initialState :: State
initialState = newGame

-- Button properties
buttonWidth = 80 :: Float
buttonHeight = 40 :: Float
startY = -290 :: Float

-- Drawing function
drawState :: State -> Picture
drawState world
  | gameStep world == 0 = Pictures $ map drawButton [0..9] ++ [stateCard]  -- Selection game mode
  | gameStep world == 1 = votingPhase  -- Voting phase
  | gameStep world == 2 = translate (-100) 0 $ scale 0.3 0.3 $ text "You Win!"  -- End game mode
  where
    buttonColors = [red, orange, yellow, green, blue, violet, cyan, magenta, rose, azure]
    drawButton i = let (x, _) = buttonPositions !! i
                       col = buttonColors !! i
                   in translate x startY $ color col $ rectangleSolid buttonWidth buttonHeight
    stateCard = Pictures [ translate 0 0 $ color white $ rectangleSolid 100 140,
                           translate 0 0 $ color black $ rectangleWire 100 140,
                           translate (-30) 0 $ scale 0.2 0.2 $ color black $ text $ "Card: " ++ (currentCardText world),
                           
                           -- submit button:
                           translate 0 (-100) $ color black $ rectangleSolid 100 40,
                           translate (-30) (-105) $ scale 0.1 0.1 $ color black $ text "Submit"]

    votingPhase = Pictures [ translate (-150) 0 $ color white $ rectangleSolid 100 140,  -- Card A
                             translate (-150) 0 $ color black $ rectangleWire 100 140,
                             translate (-180) 0 $ scale 0.2 0.2 $ color black $ text "A",
                             translate (150) 0 $ color white $ rectangleSolid 100 140,  -- Card B
                             translate (150) 0 $ color black $ rectangleWire 100 140,
                             translate (120) 0 $ scale 0.2 0.2 $ color black $ text "B" ]

-- Handling events
handleEvent :: Event -> State -> State
handleEvent (EventKey (MouseButton LeftButton) Up _ mousePos) world =
    case gameStep world of
      0 -> handleSelectionPhase mousePos world
      1 -> handleVotingPhase mousePos world
      2 -> world  -- No action in the end game phase
      _ -> world  -- Catch-all for unexpected gameStep values
  where
    handleSelectionPhase mousePos world =
      case buttonClicked mousePos of
        -- clicking a card
        Just n  -> case n of
                     -1 -> transitionToVoting world  -- Handle the submit button
                     _  -> selectCard n world  -- Handle regular card selection
        Nothing -> world

    handleVotingPhase mousePos world
      | isClickedOnCardA mousePos = voteForCardA world
      | isClickedOnCardB mousePos = voteForCardB world
      | otherwise = world

    isClickedOnCardA (x, y) = x >= -250 && x <= -50 && y >= -70 && y <= 70
    isClickedOnCardB (x, y) = x >= 50 && x <= 250 && y >= -70 && y <= 70

handleEvent _ world = world  -- Handle other events

-- Check if a button is clicked and return its index
-- also handles submit button
buttonClicked :: Point -> Maybe Int
buttonClicked (x, y)
  | x >= -250 && x <= -50 && y >= -70 && y <= 70 = Just (-1)  -- Submit -- TODO: Fix coordinates
  | otherwise = findIndex (isClicked x y) buttonPositions
  where
    isClicked xClick yClick (xPos, yPos) =
        xClick >= xPos - buttonWidth / 2 && xClick <= xPos + buttonWidth / 2 &&
        yClick >= yPos - buttonHeight / 2 && yClick <= yPos + buttonHeight / 2

-- Button positions
buttonPositions :: [(Float, Float)]
buttonPositions = [((-360) + buttonWidth * fromIntegral i, startY) | i <- [0..9]]

--- helper functions to change game state:

gameStep:: State -> Int
gameStep (State whiteCards blackCards score chosenQuestions playerHand cardSelection step) =
    step

currentCardText:: State -> Card
currentCardText (State whiteCards blackCards score chosenQuestions playerHand cardSelection step) =
    playerHand !! cardSelection

-- Transitions the game to the voting phase (sets to 1)
transitionToVoting :: State -> State
transitionToVoting (State whiteCards blackCards score chosenQuestions playerHand cardSelection _) =
    State whiteCards blackCards score chosenQuestions playerHand cardSelection 1

-- Ends the game (sets to 2)
endGame :: State -> State
endGame (State whiteCards blackCards score chosenQuestions playerHand cardSelection _) =
    State whiteCards blackCards score chosenQuestions playerHand cardSelection 2

-- Change the selected card:
selectCard :: Int -> State -> State
selectCard index (State whiteCards blackCards score chosenQuestions playerHand cardSelection gameStep) =
    State whiteCards blackCards score chosenQuestions playerHand index gameStep


-- Votes for Card A
voteForCardA :: State -> State
voteForCardA (State whiteCards blackCards (scoreW, scoreB) chosenQuestions playerHand cardSelection gameStep) =
    State whiteCards blackCards (scoreW + 1, scoreB) chosenQuestions playerHand cardSelection 0 -- Back to choosing a card phase

-- Votes for Card B
voteForCardB :: State -> State
voteForCardB (State whiteCards blackCards (scoreW, scoreB) chosenQuestions playerHand cardSelection gameStep) =
    State whiteCards blackCards (scoreW, scoreB + 1) chosenQuestions playerHand cardSelection 0 -- Back to choosing a card phase


-- Main function
main :: IO ()
main = play window white 30 initialState drawState handleEvent updateState

-- Update function, potentially needed depending on the game logic
updateState :: Float -> State -> State
updateState _ = id
