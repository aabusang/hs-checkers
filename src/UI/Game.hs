-- |
-- Module      : UI.Game
-- Description : Main game loop and state management
module UI.Game 
    ( runGame
    ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game()  -- Only import instances
import UI.Types
import UI.Input (handleInput)
import UI.Animation (updateAnimations)
import UI.Rendering (drawGameState)
import Game.State (initialGameState)
import UI.Conversion (toUIState)
import Game.Mode (GameMode(..))
import UI.Config (windowWidth, windowHeight, windowPosX, windowPosY, fps)

-- | Background color
backgroundColor :: Color
backgroundColor = white

-- | Initial game state
initialState :: GameMode -> UIState
initialState mode = UIState 
    { gameState = gameState $ toUIState initialGameState
    , selectedPosition = Nothing
    , hoverPosition = Nothing
    , lastCapture = Nothing
    , captureAnimation = 0.0
    , gameMode = mode
    }

-- | Window settings
window :: Display
window = InWindow "Checkers" 
                         (windowWidth, windowHeight)
                         (windowPosX, windowPosY)

-- | Run the game
runGame :: GameMode -> IO ()
runGame mode = do
    play window backgroundColor fps (initialState mode) drawGameState 
         handleInput updateAnimations
