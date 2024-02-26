{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Cards where

import System.Random
import Data.Array.IO
import Control.Monad
import GHC.IO.Unsafe
import Data.Char

-- state consists of:
-- the deck:
--      - white cards
--      - black cards and AI answers
-- score
-- chosen cards:
--      - game's remaining questions
--      - player's hand
-- UI views:
--      - # representing current card being displayed before being submitted
--      - # representing step of the game: 0) choosing a card, 1) voting the best card, 2) endgame view
data State = State [Card] [[Card]] (Int, Int) [Card] [Card] Int Int
         deriving (Ord, Eq, Show)

-- each card is a string of words 
type Card = String

-- type Player = State -> Action

type Game = Action -> State -> Result

-- result of a game is True if player won, False if GPT won
data Result = EndOfGame Bool State
            | ContinueGame State         
            deriving (Eq, Show)

-- player choose's which card to play
newtype Action = Action Card
        deriving (Ord, Eq, Show)

-- taken from https://www.cs.ubc.ca/~poole/cs312/2024/haskell/ReadCSV.hs
splitsep :: (a -> Bool) -> [a] -> [[a]]
splitsep sep [] = [[]]
splitsep sep (h:t)
    | sep h = []: splitsep sep t
    | otherwise = (h:w):rest
                where w:rest = splitsep sep t

-- taken from https://www.cs.ubc.ca/~poole/cs312/2024/haskell/ReadCSV.hs
-- reads csv file and turns to list of lines
readcsv :: FilePath -> IO [Card]
readcsv filename =
  do
    file <- readFile filename
    return (splitsep (=='\n') file)

-- load white deck from file 
whiteDeck :: [Card]
whiteDeck = unsafePerformIO(readcsv "answers.csv")

-- load black deck from file 
blackDeck :: [[Card]]
blackDeck = [splitsep (==',') line| line <- unsafePerformIO(readcsv "questions_answers_index.csv")]

-- algorithm for shuffle taken from https://wiki.haskell.org/Random_shuffle
-- Randomly shuffle a list
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
    where
      n = length xs
      newArray :: Int -> [a] -> IO (IOArray Int a)
      newArray n xs =  newListArray (1,n) xs

-- converts from IO deck to deck for continued use in game 
processShuffle :: [a] -> [a]
processShuffle deck = unsafePerformIO(shuffled) where shuffled = shuffle deck 

-- initialize basic game, 10 white cards, 10 black cards, 10 player's cards
newGame :: State
newGame = State w2 b (0, 0) b1 w1 cardView gameView where
    (w1, w2) = splitAt 10 (take 20 (processShuffle whiteDeck))
    b1:b = take 10 (processShuffle blackDeck)
    cardView = 0 -- initialize to first card held
    gameView = 0 -- start with card selection view

-- updates the score and refreshes black card and available actions each round
-- terminates game when there are no more white or black cards in decks
cards :: Game 
cards (Action pCard) (State (w:wCards) (b:bCards) (pScore, aScore) (card:aCard:rest) pCards cardView gameView)
    | win pCard aCard card = ContinueGame (State wCards bCards (pScore + 1, aScore) b (w:removeElem pCards pCard) cardView 0) -- TODO: where to set gameView to 1?
    | otherwise = ContinueGame (State wCards bCards (pScore, aScore+1) b (w:removeElem pCards pCard) cardView 0)
cards (Action pCard) (State _ _ (pScore, aScore) (card:aCard:rest) pCards cardView gameView)
    | win pCard aCard card = EndOfGame ((pScore + 1) > aScore) (State [] [] (pScore+1, aScore) [] pCards cardView 2)
    | otherwise = EndOfGame (pScore > (aScore+1)) (State [] [] (pScore, aScore+1) [] pCards cardView 2)

-- converts player's vote to Bool for further use
win :: Card -> Card -> Card -> Bool
win pCard aCard bCard = 
    unsafePerformIO (getPlayerVote pCard aCard bCard)

-- removes the first instance of specified element from the list
removeElem :: (Eq p) => [p] -> p -> [p]
removeElem [] _ = []
removeElem (x:xs) e
    | x == e = xs
    | otherwise = x:removeElem xs e 

-- print cards' indices so it's easier to select them (just used for command line version)
printCards :: [Card] -> Int -> IO ()
printCards [] _ = return ()
printCards (c:cs) n = do
    putStrLn $ show n ++ ": " ++ c
    printCards cs (n+1)
    
-- prints out the Black prompt card and the person's white Card choices
-- person can select 0-9 representing the order of white Cards to choose 
getPlayerChoice (State ws bs score (card:t) (p:pCards) cardView gameView) = do 
    putStrLn card
    putStrLn "Select which card to play 0-9"
    printCards pCards 0
    line <- getLine -- use # in cardView instead of player input. Change once # can be set from UI
    if all isDigit line && read line < length pCards && read line >= 0
        then return (pCards !! read line)
        else putStrLn "Invalid input, please enter a number between 0-9." >> getPlayerChoice (State ws bs score (card:t) pCards cardView gameView)

-- allows the player to vote which selected card will win 
getPlayerVote :: Card -> Card -> Card -> IO Bool
getPlayerVote pCard aCard bCard = do 
    putStrLn bCard
    putStrLn "Select which card is the funniest (0, 1)"
    print ("0: ", pCard)
    print ("1: ", aCard)
    line <- getLine 
    case line of 
        "0" -> return True
        "1" -> return False 
        _ -> getPlayerVote pCard aCard bCard 

-- loops over game play, gathering player's action and calling game to update state 
playGame :: Game -> State -> IO String
playGame game (State ws bs score (bCard:aCard:t) pCards cardView gameView) = do
    person_play <- getPlayerChoice (State ws bs score [bCard] pCards cardView gameView)
    case game (Action person_play) (State ws bs score (bCard:aCard:t) pCards cardView gameView) of 
        EndOfGame True end_state -> return "You are funnier than GPT!"
        EndOfGame False end_state -> return "GPT is funnier than you!"
        ContinueGame next_state -> playGame game next_state