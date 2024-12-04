-- |
-- Module      : UI.Types
-- Description : UI-specific types for the Checkers game
-- 
-- This module contains types specific to the user interface,
-- such as the UI state that combines game state with display options.

module UI.Types
    ( UIState(..)
    , initialUIState
    , ScreenPosition
    , BoardPosition
    ) where

import Game.State (GameState(..), initialGameState)
import Game.Mode (GameMode)
import Board.Types (Position)

-- | Screen coordinates as floating point numbers
type ScreenPosition = (Float, Float)

-- | Board coordinates as integers (0-7, 0-7)
type BoardPosition = Position


-- | The UI state containing the game state and mode
data UIState = UIState
    { gameState :: GameState  -- ^ Current game state
    , gameMode :: GameMode    -- ^ Current game mode (PvP or PvAI)
    , selectedPosition :: Maybe Position  -- ^ Currently selected position for highlighting
    } deriving (Show)

-- | Initial UI state
initialUIState :: GameMode -> UIState
initialUIState mode = UIState initialGameState mode Nothing
