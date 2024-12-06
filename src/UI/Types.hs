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

import Types.Common (Position)
import Types.Game (GameState(..))

-- | UI State for the game
data UIState = UIState
    { gameState :: GameState
    , selectedPosition :: Maybe Position  -- ^ Currently selected piece position
    } deriving (Show)

-- | Initial UI state
initialUIState :: GameState -> UIState
initialUIState gs = UIState
    { gameState = gs
    , selectedPosition = Nothing
    }
