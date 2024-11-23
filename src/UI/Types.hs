-- |
-- Module      : UI.Types
-- Description : UI-specific types for the Checkers game
-- 
-- This module contains types specific to the user interface,
-- such as the UI state that combines game state with display options.

module UI.Types
    ( UIState(..)
    , initialUIState
    ) where

import Game.State (GameState(..), GameMode(..), initialGameState)

-- | The UI state containing the game state and mode
data UIState = UIState
    { uiGameState :: GameState  -- ^ Current game state
    , uiGameMode :: GameMode    -- ^ Current game mode (PvP or PvAI)
    } deriving (Show)

-- | Initial UI state
initialUIState :: GameMode -> UIState
initialUIState mode = UIState initialGameState mode
