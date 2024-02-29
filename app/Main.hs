
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Ui
import Control.Monad (void)

-- Main function that runs the game
main :: IO ()
main = play window white 30 initialState drawState handleEvent updateState
    