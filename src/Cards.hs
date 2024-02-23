import GHC.IO.Unsafe

-- state consists of internal state and the score of the Player and the AI
data State = State InternalState (Card, Card) [Card]
         deriving (Ord, Eq, Show)

-- internal state consists of the white cards to draw from, the black cards to draw from 
-- (alongside GPT's best answer) and the current score (players score, GPT's score)
type InternalState = ([Card], [(Card, Card)], (Int, Int))

-- each card is a string of words 
type Card = String

type Player = State -> Action

type Game = Action -> State -> Result

-- result of a game is True if player won, False if AI won
data Result = EndOfGame Bool State
            | ContinueGame State         
            deriving (Eq, Show)

-- both players choose a card to play 
newtype Action = Action Card
        deriving (Ord, Eq, Show)

-- start of hands
-- !!! TODO: initialize properly
fullDeck :: [Card]
fullDeck = ["s", "a", "b", "d", "e"]

-- initialized basic game
-- !!! TODO: intialize properly
newGame :: State
newGame = State (fullDeck, [("x", "a")], (0, 0)) ("d", "e") fullDeck

-- ([Card], [(Card, Card)], (Card, Card), (Int, Int)) [Card]
-- updates the score each round, terminates game when there are no more white/black cards to use
cards :: Game 
cards (Action pCard) (State (w:wCards, (b, a):bCards, (pScore, aScore)) (card, aCard) pCards)
    | win pCard aCard card = ContinueGame (State (wCards, bCards, (pScore + 1, aScore)) (b, a) (w:removeElem pCards pCard))
    | otherwise = ContinueGame (State (wCards, bCards, (pScore, aScore+1)) (b, a) (w:removeElem pCards pCard))
cards (Action pCard) (State (_, _, (pScore, aScore)) (card, aCard) pCards)
    | win pCard aCard card = EndOfGame ((pScore + 1) > aScore) (State ([], [], (pScore+1, aScore)) ("", "") pCards)
    | otherwise = EndOfGame (pScore > (aScore+1)) (State ([], [], (pScore, aScore+1)) ("", "") pCards)

-- converts player's vote to Bool (instead of IO Bool)
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

-- loops over game play, gathering both players choices into one action and calling game to update state 
play :: Game -> State -> IO String
play game (State internalstate (bCard, aCard) pCards) = do
    --print score 
    person_play <- person (State internalstate (bCard, "") pCards)
    case game (Action person_play) (State internalstate (bCard, aCard) pCards) of 
        EndOfGame True end_state -> return "You are funnier than GPT!"
        EndOfGame False end_state -> return "GPT is funnier than you!"
        ContinueGame next_state -> play game next_state
                             
-- prints out the Black prompt card and the person's white Card choices
-- person can select 0-9 representing the order of white Cards to choose 
-- !!! TODO: add checks for proper input and return user specified card, currently returns the first               
person :: State -> IO Card
person (State internalstate (card, _) (p:pCards)) = do 
    putStrLn card
    putStrLn "Select which card to play 0-9"
    print (p:pCards)
    line <- getLine
    return p
    -- let nums = ["0", "1", "2", "3", "4", "5", "6"]
    --return (process_selection pCards nums line)

-- !!! TODO: fix  attempt at input checking 
{- process_selection :: [Card] -> [String] -> String -> Card -}
{- process_selection (c:cards) (n:nums) line  -}
{-     | n == line = return c -}
{-     | otherwise = process_selection cards nums line -}
{- process_selection _ _ _ = return "" -}
{-      -}
