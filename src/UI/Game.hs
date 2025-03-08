-- |
-- Module      : UI.Game
-- Description : Main game loop and state management
module UI.Game 
    ( runGame
    ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Game (playIO)
import UI.Types
import UI.Input (handleInput)
import UI.Animation (updateAnimations)
import UI.Rendering (drawGameState)
import Game.State (initialGameState)
import UI.Conversion (toUIState)
import Game.Mode (GameMode(..))
import UI.Config (windowWidth, windowHeight, windowPosX, windowPosY, fps)
import UI.AIIntegration (processAITurn)

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
    , aiThinkingState = Idle
    }

-- | Window settings
window :: Display
window = InWindow "Checkers" 
                         (windowWidth, windowHeight)
                         (windowPosX, windowPosY)

-- | Run the game
runGame :: GameMode -> IO ()
runGame mode = do
    playIO window backgroundColor fps (initialState mode) 
          drawGameStateIO handleInputIO updateGameWithAI
    
-- | IO version of drawGameState for playIO
drawGameStateIO :: UIState -> IO Picture
drawGameStateIO = return . drawGameState

-- | IO version of handleInput for playIO
handleInputIO :: Event -> UIState -> IO UIState
handleInputIO event uiState = return $ handleInput event uiState

-- | Update game state with AI processing
updateGameWithAI :: Float -> UIState -> IO UIState
updateGameWithAI dt uiState = do
    -- First apply regular animations
    let updatedState = updateAnimations dt uiState
    -- Then process AI turn if needed
    processAITurn dt updatedState
