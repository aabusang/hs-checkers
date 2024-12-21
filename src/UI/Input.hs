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

-- | Handle all input events
handleInput :: Event -> UIState -> UIState
handleInput (EventKey (MouseButton LeftButton) Down _ screenPos) uiState =
    case screenToBoardPosition screenPos of
        Just boardPos -> handleMouseClick uiState boardPos
        Nothing -> uiState
handleInput (EventKey (SpecialKey KeyEsc) Down _ _) uiState = 
    clearSelection uiState
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