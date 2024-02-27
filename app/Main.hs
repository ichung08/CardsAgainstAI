import Cards
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Ui (State, initialState, render, handleEvents, update)
import Testui
import Control.Monad (void)

-- main :: IO ()
-- main = play window background fps initialState render handleEvents update
--   where
--     window = InWindow "Cards Against AI" (1024, 768) (100, 100)
--     background = white
--     fps = 60 -- Frames per second

-- play game in window
main :: IO ()
main = play window white 30 initialUiState drawWorld handleEvent updateWorld

-- play game in command line
-- main :: IO ()
-- main = void $ playGame cards newGame