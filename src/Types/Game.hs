-- |
-- Module      : Types.Game
-- Description : Game-related types
-- 
-- This module contains the core game types used across the application.

module Types.Game
    ( GameState(..)
    , GameMode(..)
    , Difficulty(..)
    ) where

import Board.Types (Board, Player(..), Position)

-- | Represents the current state of the game
data GameState = GameState
    { board         :: Board            -- ^ The current board configuration
    , currentPlayer :: Player           -- ^ The player whose turn it is
    , selectedPiece :: Maybe Position   -- ^ The currently selected piece's position, if any
    } deriving (Show, Eq)

-- | Represents the AI difficulty levels
data Difficulty = Easy    -- ^ AI makes random valid moves
                | Medium  -- ^ AI looks ahead 1-2 moves
                | Hard    -- ^ AI looks ahead 2-3 moves and uses better evaluation
                deriving (Show, Eq)

-- | Represents the game mode
data GameMode = SinglePlayer Difficulty  -- ^ Play against AI with specified difficulty
              | TwoPlayer               -- ^ Two human players
              deriving (Show, Eq)
