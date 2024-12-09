module UI.Input
    ( handleInput
    , InputEvent(..)
    ) where

import Graphics.Gloss.Interface.Pure.Game 
    ( Event(..)
    , Key(..)
    , SpecialKey(..)
    , MouseButton(..)
    , KeyState(..)
    )
import Game.State (selectPiece, makeMove)
import UI.Types
import UI.Shared (screenToBoardPosition)
import UI.Conversion (toUIState, fromUIPosition, fromUIState)

-- | Custom input events
data InputEvent 
    = MouseClick UIPosition
    | KeyPress Key
    | NoEvent
    deriving (Show, Eq)

-- | Handle all input events
handleInput :: UIState -> Event -> UIState
handleInput uiState event =
    case convertEvent uiState event of
        MouseClick pos -> handleMouseClick pos uiState
        KeyPress (SpecialKey KeyEsc) -> clearSelection uiState
        KeyPress _ -> uiState  -- Handle all other key presses
        NoEvent -> uiState

-- | Convert Gloss event to our InputEvent type
convertEvent :: UIState -> Event -> InputEvent
convertEvent uiState (EventKey (MouseButton LeftButton) Down _ pos) = 
    let board = uiBoard (gameState uiState)
    in case screenToBoardPosition board pos of
        Just boardPos -> MouseClick boardPos
        Nothing -> NoEvent
convertEvent _ (EventKey key Down _ _) = KeyPress key
convertEvent _ _ = NoEvent

-- | Handle mouse click at a board position
handleMouseClick :: UIPosition -> UIState -> UIState
handleMouseClick pos uiState =
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
                   , selectedPosition = Nothing
                   , lastCapture = Nothing
                   , captureAnimation = 0.0
                   }
        Nothing -> clearSelection uiState

-- | Clear the current selection
clearSelection :: UIState -> UIState
clearSelection uiState = uiState { selectedPosition = Nothing }