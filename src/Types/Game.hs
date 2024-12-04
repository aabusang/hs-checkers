-- |
-- Module      : Types.Game
-- Description : Core game state type
-- 
-- This module contains the GameState type which represents the current state
-- of a checkers game.

module Types.Game
    ( GameState(..)
    ) where

import Board.Types (Board, Player(..), Position)

-- | Represents the current state of the game
data GameState = GameState
    { currentBoard  :: Board            -- ^ The current board configuration
    , currentPlayer :: Player           -- ^ The player whose turn it is
    , selectedPiece :: Maybe Position   -- ^ The currently selected piece's position, if any
    } deriving (Show, Eq)
