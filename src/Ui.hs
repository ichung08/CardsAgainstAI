{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Ui where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.List (findIndex)

import Cards

-- Defines the window properties for the GUI
window :: Display
window = InWindow "Cards Against AI" (1440, 768) (100, 100)

-- Initializes the game state using the newGame function from the Cards module
initialState :: State
initialState = newGame

-- Button properties
buttonWidth = 80 :: Float
buttonHeight = 40 :: Float
startY = -290 :: Float

-- The main drawing function that renders the game state to the screen
-- It checks the current game step and displays the corresponding game phase:
-- a selection phase with buttons, a voting phase with options, or the end game text
drawState :: State -> Picture
drawState world
  | gameStep world == 0 = Pictures $ map drawButton [0..9] ++ [selectionPhase]  -- Selection game mode
  | gameStep world == 1 = votingPhase  -- Voting phase
  | gameStep world == 2 = translate (-100) 0 $ scale 0.3 0.3 $ text (endText world)  -- End game mode
  where
    buttonColors = [red, orange, yellow, green, blue, violet, cyan, magenta, rose, azure]
    drawButton i = let (x, _) = buttonPositions !! i
                       col = buttonColors !! i
                   in translate x startY $ color col $ rectangleSolid buttonWidth buttonHeight
    
    -- The selection phase layout, which includes the current question, and the current card
    selectionPhase = Pictures [ translate 0 0 $ color white $ rectangleSolid 400 140,
                              translate (-550) (0) $ scale 0.15 0.15 $ color black $ text $ "Card: " ++ (currentCardText world), -- -200 moves text left
                              
                              -- submit button:
                              translate 0 (-100) $ color black $ rectangleWire 100 40,
                              translate (-30) (-105) $ scale 0.1 0.1 $ color black $ text "Submit",
                              
                              -- current question
                              translate (-550) (100) $ scale 0.16 0.16 $ color black $ text $ (currentQText world)]
    
    -- The voting phase layout, which includes two larger rectangles representing the cards to vote on,
    -- with text indicating the user's card and the AI's card
    votingPhase = Pictures [ translate (-550) (100) $ scale 0.16 0.16 $ color black $ text $ (currentQText world), -- current question
                             
                             translate (-515) (10) $ color black $ rectangleWire 100 70, -- Card A
                             translate (-550) (0) $ scale 0.15 0.15 $ color black $ text $ "Card A: " ++ (currentCardText world),

                             translate (-515) (-90) $ color black $ rectangleWire 100 70, -- Card B
                             translate (-550) (-100) $ scale 0.15 0.15 $ color black $ text $ "Card B: " ++ (currentQAnswer world)]


-- Handles user input events and updates the game state accordingly
handleEvent :: Event -> State -> State
handleEvent (EventKey (MouseButton LeftButton) Up _ mousePos) world =
    case gameStep world of
      0 -> handleSelectionPhase mousePos world  -- In the selection phase, handle the event based on the button clicked
      1 -> handleVotingPhase mousePos world     -- In the voting phase, handle the event based on voting card selection
      2 -> world  -- No action is taken if the game is in the end game phase
      _ -> world  
  where
    -- Responds to click events during the selection phase by selecting a card or transitioning to the voting phase
    handleSelectionPhase mousePos world =
      case buttonClicked mousePos of
        Just n  -> case n of
                     -1 -> transitionToVoting world  -- If the submit button is clicked, proceed to the voting phase
                     _  -> selectCard n world        -- If any other button is clicked, select the corresponding card
        Nothing -> world  

    -- Responds to click events during the voting phase by processing votes for Card A or Card B
    handleVotingPhase mousePos world
      | isClickedOnCardA mousePos = voteForCardA world  -- If Card A is clicked, register a vote for Card A
      | isClickedOnCardB mousePos = voteForCardB world  -- If Card B is clicked, register a vote for Card B
      | otherwise = world  

    -- Checks if Card A is clicked based on the mouse position
    isClickedOnCardA (x, y) = x >= -575 && y >= 0 && y <= 90

    -- Checks if Card B is clicked based on the mouse position
    isClickedOnCardB (x, y) = x >= -575 && y >= -100 && y <= -10

-- Default case for handling events, which maintains the current state for any non-mouse events
handleEvent _ world = world

-- Determines if a mouse click is within the bounds of any button or the submit area, returning the index of the button clicked
buttonClicked :: Point -> Maybe Int
buttonClicked (x, y)
  | x >= -50 && x <= 50 && y >= -120 && y <= -80 = Just (-1)  -- Checks if the submit button is clicked
  | otherwise = findIndex (isClicked x y) buttonPositions       -- Checks which button, if any, is clicked
  where
    isClicked xClick yClick (xPos, yPos) =
        xClick >= xPos - buttonWidth / 2 && xClick <= xPos + buttonWidth / 2 &&
        yClick >= yPos - buttonHeight / 2 && yClick <= yPos + buttonHeight / 2

-- A list of tuples representing the positions of buttons on the screen
buttonPositions :: [(Float, Float)]
buttonPositions = [((-360) + buttonWidth * fromIntegral i, startY) | i <- [0..9]]

--- helper functions to change game state:

-- Retrieves the current game step from the State
gameStep :: State -> Int
gameStep (State _ _ _ _ _ _ step) = step

-- Extracts the text of the currently selected card from the player's hand within the State
currentCardText :: State -> Card
currentCardText (State _ _ _ _ playerHand cardSelection _) = playerHand !! cardSelection

-- Obtains the text of the current question from the State
currentQText :: State -> Card
currentQText (State _ _ _ (currQ:_) _ _ _) = currQ

-- Gets the answer text associated with the current question from the State
currentQAnswer :: State -> Card
currentQAnswer (State _ _ _ (currQ:currQAns:_) _ _ _) = currQAns

-- Determines the end game text based on the scores within the State, indicating a win or loss
endText :: State -> [Char]
endText (State _ _ (pScore,aScore) _ _ _ _) =
  if pScore >= aScore then "You win!" else "You lose"

-- Changes the State to transition the game to the voting phase
transitionToVoting :: State -> State
transitionToVoting (State whiteCards blackCards score chosenQuestions playerHand cardSelection _) =
    State whiteCards blackCards score chosenQuestions playerHand cardSelection 1

-- Modifies the State to indicate that the game has ended
endGame :: State -> State
endGame (State whiteCards blackCards score chosenQuestions playerHand cardSelection _) =
    State whiteCards blackCards score chosenQuestions playerHand cardSelection 2

-- Updates the State to reflect the selection of a new card by the player
selectCard :: Int -> State -> State
selectCard index (State whiteCards blackCards score chosenQuestions playerHand _ gameStep) =
    State whiteCards blackCards score chosenQuestions playerHand index gameStep

-- main game updating here:
-- increment score based on choice
-- remove move top of Q/A decks into chosenQuestions and playerHand
-- set mode back to card selection or to endGame

-- Processes a vote for Card A, updates the game state accordingly
-- It increments the player's score, removes the selected card from the hand,
-- moves the top of the question/answer decks into the chosenQuestions, and resets the game step for the next round
voteForCardA :: State -> State
voteForCardA (State (w:wCards) (b:bCards) (pScore, aScore) (currQ:chosenQuestions) playerHand cardSelection gameStep) =
  State wCards bCards (pScore+1, aScore) b (w:removeAt cardSelection playerHand) cardSelection 0
-- If there are no more questions left, it ends the game and sets the game step to 2
voteForCardA (State (w:wCards) [] (pScore, aScore) (currQ:chosenQuestions) playerHand cardSelection gameStep) =
  State (w:wCards) [] (pScore+1, aScore) [] playerHand cardSelection 2
-- In case of a bug or unexpected situation where the game cannot continue normally, it forces the game to end
voteForCardA (State _ _ (pScore, aScore) _ _ _ gameStep) = State [] [] (pScore+1, aScore) [] [] 0 2

-- Processes a vote for Card B, and increments the AI's score instead
voteForCardB :: State -> State
voteForCardB (State (w:wCards) (b:bCards) (pScore, aScore) (currQ:chosenQuestions) playerHand cardSelection gameStep) =
  State wCards bCards (pScore, aScore+1) b (w:removeAt cardSelection playerHand) cardSelection 0
-- Ends the game if no more questions are available, indicating the round of voting is the last
voteForCardB (State _ _ (pScore, aScore) (currQ:chosenQuestions) playerHand cardSelection gameStep) =
  State [] [] (pScore, aScore+1) [] playerHand cardSelection 2
-- Forces the game to the end screen if the game's state is invalid or cannot continue
voteForCardB (State _ _ (pScore, aScore) _ _ _ gameStep) = State [] [] (pScore, aScore+1) [] [] 0 2

-- Updates game state
updateState :: Float -> State -> State
updateState _ = id