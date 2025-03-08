module UI.Input
    ( handleInput
    ) where

import Graphics.Gloss.Interface.Pure.Game 
    ( Event(..)
    , Key(..)
    , SpecialKey(..)
    , MouseButton(..)
    , KeyState(Down)
    )
import Game.State (selectPiece, makeMove, selectedPiecePos)
import UI.Types
import UI.Shared (screenToBoardPosition)
import UI.Conversion (toUIState, fromUIPosition, fromUIState, toUIPosition)
import AI.Types (Difficulty(..))
import Game.Mode (GameMode(..))
import UI.Config (sidebarWidth, sidebarMargin, windowWidth, windowHeight)

-- | Handle all input events
handleInput :: Event -> UIState -> UIState
handleInput (EventKey (MouseButton LeftButton) Down _ screenPos) uiState =
    -- Check if click is in the sidebar difficulty buttons area
    if isSidebarClick screenPos && gameMode uiState == SinglePlayer
    then handleSidebarClick uiState screenPos
    -- Otherwise handle as a board click
    else case screenToBoardPosition screenPos of
        Just boardPos -> handleMouseClick uiState boardPos
        Nothing -> uiState
handleInput (EventKey (SpecialKey KeyEsc) Down _ _) uiState = 
    clearSelection uiState
-- Handle AI difficulty changes
handleInput (EventKey (Char '1') Down _ _) uiState = 
    uiState { aiDifficulty = Easy }
handleInput (EventKey (Char '2') Down _ _) uiState = 
    uiState { aiDifficulty = Medium }
handleInput (EventKey (Char '3') Down _ _) uiState = 
    uiState { aiDifficulty = Hard }
handleInput _ uiState = uiState

-- | Handle mouse click at a board position
handleMouseClick :: UIState -> UIPosition -> UIState
handleMouseClick uiState pos =
    if isPositionSelected uiState
    then handleMovement uiState pos
    else handleSelection uiState pos

-- | Check if a position is currently selected
isPositionSelected :: UIState -> Bool
isPositionSelected uiState = selectedPosition uiState /= Nothing

-- | Handle piece selection
handleSelection :: UIState -> UIPosition -> UIState
handleSelection uiState pos =
    let gs = fromUIState uiState
    in case selectPiece gs (fromUIPosition pos) of
        Just newGameState -> 
            uiState { gameState = gameState $ toUIState newGameState
                   , selectedPosition = Just pos 
                   }
        Nothing -> uiState  -- Do nothing for invalid selections

-- | Handle piece movement
handleMovement :: UIState -> UIPosition -> UIState
handleMovement uiState targetPos =
    case selectedPosition uiState of
        Nothing -> uiState
        Just fromPos -> tryMove uiState fromPos targetPos

-- | Try to move a piece
tryMove :: UIState -> UIPosition -> UIPosition -> UIState
tryMove uiState fromPos toPos =
    let gs = fromUIState uiState
    in case makeMove gs (fromUIPosition fromPos) (fromUIPosition toPos) of
        Just newGameState -> 
            uiState { gameState = gameState $ toUIState newGameState
                   , selectedPosition = case selectedPiecePos newGameState of
                                        Just pos -> Just $ toUIPosition pos
                                        Nothing -> Nothing
                   , lastCapture = Just toPos
                   , captureAnimation = 0.0
                   }
        Nothing -> clearSelection uiState

-- | Clear the current selection
clearSelection :: UIState -> UIState
clearSelection uiState = uiState { selectedPosition = Nothing }

-- | Check if a click is in the sidebar area
isSidebarClick :: (Float, Float) -> Bool
isSidebarClick (x, y) = x < -fromIntegral windowWidth / 2 + sidebarWidth

-- | Handle clicks in the sidebar
handleSidebarClick :: UIState -> (Float, Float) -> UIState
handleSidebarClick uiState (x, y) =
    -- Calculate sidebar coordinates
    let sidebarX = -fromIntegral windowWidth / 2 + sidebarMargin
        sidebarTop = fromIntegral windowHeight / 2 - sidebarMargin
        
        -- Difficulty buttons area
        buttonY = sidebarTop - 200
        buttonWidth = 50
        buttonHeight = 30
        buttonSpacing = 60
        
        -- Button positions
        easyButtonX = sidebarX + 30
        mediumButtonX = sidebarX + 30 + buttonSpacing
        hardButtonX = sidebarX + 30 + 2*buttonSpacing
        
        -- Check if click is within a button's bounds
        isInButton centerX = 
            x >= centerX - buttonWidth/2 && 
            x <= centerX + buttonWidth/2 && 
            y >= buttonY - buttonHeight/2 && 
            y <= buttonY + buttonHeight/2
    in
        -- Set difficulty based on which button was clicked
        if isInButton easyButtonX then 
            uiState { aiDifficulty = Easy }
        else if isInButton mediumButtonX then 
            uiState { aiDifficulty = Medium }
        else if isInButton hardButtonX then 
            uiState { aiDifficulty = Hard }
        else 
            uiState  -- Click was in sidebar but not on a button