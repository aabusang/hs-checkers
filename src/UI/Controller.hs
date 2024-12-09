-- |
-- Module      : UI.Controller
-- Description : UI state management and input handling
module UI.Controller 
    ( -- * Input Handling
      handleInput
    , handleMouseClick
    , handleMouseMove
    , handleKeyPress
    , clearSelection
    ) where

import Graphics.Gloss.Interface.Pure.Game (Key(..), SpecialKey(..))
import UI.Types
import UI.Input (InputEvent(..))
import Game.State (selectPiece, makeMove)
import UI.Conversion (toUIState, fromUIState, fromUIPosition)

-- | Handle all input events
handleInput :: UIState -> InputEvent -> UIState
handleInput state event =
    case event of
        MouseClick pos -> handleMouseClick state pos
        KeyPress (SpecialKey KeyEsc) -> clearSelection state
        KeyPress _ -> state  -- Handle all other keys
        NoEvent -> state

-- | Handle mouse click at a board position
handleMouseClick :: UIState -> UIPosition -> UIState
handleMouseClick state pos =
    if isPositionSelected state
    then handleMovement state pos
    else handleSelection state pos

-- | Handle mouse movement
handleMouseMove :: UIState -> UIPosition -> UIState
handleMouseMove state pos =
    state { hoverPosition = Just pos }

-- | Handle key press events
handleKeyPress :: UIState -> Key -> UIState
handleKeyPress state key =
    case key of
        SpecialKey KeyEsc -> clearSelection state
        _ -> state

-- | Clear the current selection
clearSelection :: UIState -> UIState
clearSelection state = state { selectedPosition = Nothing }

-- | Check if a position is currently selected
isPositionSelected :: UIState -> Bool
isPositionSelected state = selectedPosition state /= Nothing

-- | Handle piece selection
handleSelection :: UIState -> UIPosition -> UIState
handleSelection state pos =
    let gs = fromUIState state
    in case selectPiece gs (fromUIPosition pos) of
        Just newGameState -> 
            state { gameState = gameState $ toUIState newGameState
                 , selectedPosition = Just pos 
                 }
        Nothing -> state

-- | Handle piece movement
handleMovement :: UIState -> UIPosition -> UIState
handleMovement state targetPos =
    case selectedPosition state of
        Nothing -> state
        Just fromPos -> tryMove state fromPos targetPos

-- | Try to move a piece
tryMove :: UIState -> UIPosition -> UIPosition -> UIState
tryMove state fromPos toPos =
    let gs = fromUIState state
    in case makeMove gs (fromUIPosition fromPos) (fromUIPosition toPos) of
        Just newGameState -> 
            state { gameState = gameState $ toUIState newGameState
                 , selectedPosition = Nothing
                 , lastCapture = Nothing
                 , captureAnimation = 0.0
                 }
        Nothing -> clearSelection state
