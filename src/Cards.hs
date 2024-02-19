import GHC.IO.Unsafe

-- state consists of internal state and the score of the Player and the AI
data State = State InternalState (Int, Int)
         deriving (Ord, Eq, Show)

-- internal state consists of the player's cards, the AI's cards, the white 
-- cards to draw from and the black cards to draw from and the current black card
type InternalState = ([Card], [Card], [Card], [Card], Card)

-- each card is a string of words 
type Card = String

type Player = State -> Action

type Game = Action -> State -> Result

-- result of a game is True if player won, False if AI won
data Result = EndOfGame Bool State
            | ContinueGame State         
            deriving (Eq, Show)

-- both players choose a card to play 
data Action = Pair (Card, Card)
         deriving (Ord, Eq, Show)

-- start of hands
-- !!! TODO: initialize properly
fullDeck :: [Card]
fullDeck = ["s", "a", "b", "d", "e"]

-- initialized basic game
-- !!! TODO: intialize properly
newGame :: State
newGame = State (["y"], ["x"], fullDeck, fullDeck, "d") (0, 0)

-- updates the score each round, terminates game when there are no more white/black cards to use
cards :: Game 
cards (Pair (pCard, aCard))  (State (pCards, aCards, w1:w2:wCards, b:bCards, card) (pScore, aScore)) 
    | win pCard aCard card = ContinueGame (State (w1:(remove_elem pCards pCard), w2:(remove_elem aCards aCard), wCards, bCards, b) (pScore + 1, aScore))
    | otherwise = ContinueGame (State (w1:(remove_elem pCards pCard), w2:(remove_elem aCards aCard), wCards, bCards, b) (pScore, aScore + 1))
cards (Pair (pCard, aCard))  (State (pCards, aCards, _, _, card) (pScore, aScore))
    | win pCard aCard card = EndOfGame ((pScore + 1) > aScore) (State ((remove_elem pCards pCard), (remove_elem aCards aCard), [], [], card) (pScore + 1, aScore))
    | otherwise = EndOfGame (pScore > (aScore+1)) (State ((remove_elem pCards pCard), (remove_elem aCards aCard), [], [], card) (pScore, aScore + 1))

-- converts player's vote to Bool (instead of IO Bool)
win :: Card -> Card -> Card -> Bool
win pCard aCard bCard = 
    unsafePerformIO (get_player_vote pCard aCard bCard)

-- allows the player to vote which selected card will win 
get_player_vote :: Card -> Card -> Card -> IO Bool
get_player_vote pCard aCard bCard = do 
    putStrLn bCard
    putStrLn "Select which card is the funniest (0, 1)"
    print ("0: ", pCard)
    print ("1: ", aCard)
    line <- getLine 
    case line of 
        "0" -> return True
        "1" -> return False 
        _ -> get_player_vote pCard aCard bCard 

-- removes the first instance of specified element from the list
remove_elem :: (Eq p) => [p] -> p -> [p]
remove_elem [] _ = []
remove_elem (x:xs) e
    | x == e = xs
    | otherwise = x:remove_elem xs e 

-- loops over game play, gathering both players choices into one action and calling game to update state 
play :: Game -> State -> IO (Int, Int)
play game (State internalstate score) = do
    print score 
    person_play <- person (State internalstate score)
    ai_play <- ai (State internalstate score)
    case game (Pair (person_play, ai_play)) (State internalstate score) of 
        EndOfGame _ (State end_state end_score) -> return end_score
        ContinueGame next_state -> play game next_state
                             
-- prints out the Black prompt card and the person's white Card choices
-- person can select 0-9 representing the order of white Cards to choose 
-- !!! TODO: add checks for proper input and return user specified card, currently returns the first               
person :: State -> IO Card
person (State (p:pCards, x, y, z, card) (pScore, aScore)) = do 
    putStrLn card
    putStrLn "Select which card to play 0-9"
    print (p:pCards)
    line <- getLine
    return p
    -- let nums = ["0", "1", "2", "3", "4", "5", "6"]
    --return (process_selection pCards nums line)

-- returns the AI's selected card 
-- !!! TODO: implement API call, currently just returns first card
ai :: State -> IO Card 
ai (State (w, a:aCards, y, z, card) (pScore, aScore)) = do
    return a

-- !!! TODO: fix  attempt at input checking 
{- process_selection :: [Card] -> [String] -> String -> Card -}
{- process_selection (c:cards) (n:nums) line  -}
{-     | n == line = return c -}
{-     | otherwise = process_selection cards nums line -}
{- process_selection _ _ _ = return "" -}
{-      -}

