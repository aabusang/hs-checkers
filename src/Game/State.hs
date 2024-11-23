-- |
-- Module      : Game.State
-- Description : Game state management
-- 
-- This module handles the game state, including the current board configuration,
-- active player, and selected piece.

module Game.State
    ( -- * Re-exports from Types.Game
      module Types.Game
    , initialGameState
    ) where

import Board.Types (Player(..))
import Board.Construction (initialBoard)
import Types.Game

-- | Creates the initial game state.
-- The game starts with Black player's turn and no piece selected.
initialGameState :: GameState
initialGameState = GameState initialBoard Black Nothing
