{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
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
  | gameStep world == 0 = Pictures $ map drawButton [0..9] ++ [selectionPhase]  -- Selection game mode
  | gameStep world == 1 = votingPhase  -- Voting phase
  | gameStep world == 2 = translate (-100) 0 $ scale 0.3 0.3 $ text (endText world)  -- End game mode
  where
    buttonColors = [red, orange, yellow, green, blue, violet, cyan, magenta, rose, azure]
    drawButton i = let (x, _) = buttonPositions !! i
                       col = buttonColors !! i
                   in translate x startY $ color col $ rectangleSolid buttonWidth buttonHeight
    selectionPhase = Pictures [ translate 0 0 $ color white $ rectangleSolid 400 140,
                           translate 0 0 $ color black $ rectangleWire 400 140,
                           translate (-350) 0 $ scale 0.2 0.2 $ color black $ text $ "Card: " ++ (currentCardText world), -- -200 moves text left
                           
                           -- submit button:
                           translate 0 (-100) $ color white $ rectangleSolid 100 40,
                           translate (-30) (-105) $ scale 0.1 0.1 $ color black $ text "Submit",
                           
                           -- current question
                           translate (-350) (100) $ scale 0.2 0.2 $ color black $ text $ "Question: " ++ (currentQText world)]

    votingPhase = Pictures [ translate (-150) 0 $ color white $ rectangleSolid 100 140,  -- Card A
                             translate (-150) 0 $ color black $ rectangleWire 100 140,
                             translate (-180) 0 $ scale 0.2 0.2 $ color black $ text (currentCardText world),
                             translate (150) 0 $ color white $ rectangleSolid 100 140,  -- Card B
                             translate (150) 0 $ color black $ rectangleWire 100 140,
                             translate (120) 0 $ scale 0.2 0.2 $ color black $ text (currentQAnswer world) ]

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
  | x >= -50 && x <= 50 && y >= -140 && y <= -100 = Just (-1)  -- Submit
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

currentQText:: State -> Card
currentQText (State whiteCards blackCards score (currQ:chosenQuestions) playerHand cardSelection step) =
    takeBeforeSemicolon currQ

currentQAnswer:: State -> Card
currentQAnswer (State whiteCards blackCards score (currQ:chosenQuestions) playerHand cardSelection step) =
    takeAfterSemicolon currQ

endText:: State -> [Char]
endText (State whiteCards blackCards (pScore,aScore) chosenQuestions playerHand cardSelection _)
  | pScore >= aScore = "You win!"
  | otherwise = "You lose"

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

-- main game updating here:
-- increment score based on choice
-- remove move top of Q/A decks into chosenQuestions and playerHand
-- set mode back to card selection or to endGame

-- Votes for Card A
voteForCardA :: State -> State
voteForCardA (State (w:wCards) (b:bCards) (pScore, aScore) (currQ:chosenQuestions) playerHand cardSelection gameStep) =
  State wCards bCards (pScore+1, aScore) b (w:removeAt cardSelection playerHand) cardSelection 0
-- end game if questions are empty:
voteForCardA (State (w:wCards) [] (pScore, aScore) (currQ:chosenQuestions) playerHand cardSelection gameStep) =
  State (w:wCards) [] (pScore+1, aScore) [] playerHand cardSelection 2
-- catch bugs, skip to end screen
voteForCardA (State _ _ (pScore, aScore) _ _ _ gameStep) = (State [] [] (pScore+1, aScore) [] [] 0 2)

-- Votes for Card B
voteForCardB :: State -> State
voteForCardB (State (w:wCards) (b:bCards) (pScore, aScore) (currQ:chosenQuestions) playerHand cardSelection gameStep) =
  State wCards bCards (pScore, aScore+1) b (w:removeAt cardSelection playerHand) cardSelection 0
-- end game if questions are empty:
voteForCardB (State _ _ (pScore, aScore) (currQ:chosenQuestions) playerHand cardSelection gameStep) =
  State [] [] (pScore, aScore+1) [] playerHand cardSelection 2
voteForCardB (State _ _ (pScore, aScore) _ _ _ gameStep) = (State [] [] (pScore, aScore+1) [] [] 0 2)



-- Main function
main :: IO ()
main = play window white 30 initialState drawState handleEvent updateState

-- Update function, potentially needed depending on the game logic
updateState :: Float -> State -> State
updateState _ = id
