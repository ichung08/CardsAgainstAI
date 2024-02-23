import System.Random
import Data.Array.IO
import Control.Monad
import GHC.IO.Unsafe

-- state consists of internal state and the score of the Player and the AI
data State = State InternalState [Card] [Card]
         deriving (Ord, Eq, Show)

-- internal state consists of the white cards to draw from, the black cards to draw from 
-- alongside GPT's best answer and the current score (players score, GPT's score)
type InternalState = ([Card], [[Card]], (Int, Int))

-- each card is a string of words 
type Card = String

type Player = State -> Action

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
newGame = State (w2, b, (0, 0)) b1 w1 where 
    (w1, w2) = splitAt 10 (take 20 (processShuffle whiteDeck))
    b1:b = take 10 (processShuffle blackDeck)


-- updates the score and refreshes black card and available actions each round
-- terminates game when there are no more white or black cards in decks
cards :: Game 
cards (Action pCard) (State (w:wCards, b:bCards, (pScore, aScore)) (card:aCard:rest) pCards)
    | win pCard aCard card = ContinueGame (State (wCards, bCards, (pScore + 1, aScore)) b (w:removeElem pCards pCard))
    | otherwise = ContinueGame (State (wCards, bCards, (pScore, aScore+1)) b (w:removeElem pCards pCard))
cards (Action pCard) (State (_, _, (pScore, aScore)) (card:aCard:rest) pCards)
    | win pCard aCard card = EndOfGame ((pScore + 1) > aScore) (State ([], [], (pScore+1, aScore)) [] pCards)
    | otherwise = EndOfGame (pScore > (aScore+1)) (State ([], [], (pScore, aScore+1)) [] pCards)

-- converts player's vote to Bool for further use
win :: Card -> Card -> Card -> Bool
win pCard aCard bCard = 
    unsafePerformIO (getPlayerVote pCard aCard bCard)

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

-- removes the first instance of specified element from the list
removeElem :: (Eq p) => [p] -> p -> [p]
removeElem [] _ = []
removeElem (x:xs) e
    | x == e = xs
    | otherwise = x:removeElem xs e 

-- loops over game play, gathering player's action and calling game to update state 
play :: Game -> State -> IO String
play game (State internalstate (bCard:aCard:t) pCards) = do
    person_play <- person (State internalstate [bCard] pCards)
    case game (Action person_play) (State internalstate (bCard:aCard:t) pCards) of 
        EndOfGame True end_state -> return "You are funnier than GPT!"
        EndOfGame False end_state -> return "GPT is funnier than you!"
        ContinueGame next_state -> play game next_state
                             
-- prints out the Black prompt card and the person's white Card choices
-- person can select 0-9 representing the order of white Cards to choose 
-- !!! TODO: add checks for proper input and return user specified card, currently returns the first               
person :: State -> IO Card
person (State internalstate (card:t) (p:pCards)) = do 
    putStrLn card
    putStrLn "Select which card to play 0-9"
    print (p:pCards)
    line <- getLine
    return p
