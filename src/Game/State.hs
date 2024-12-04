-- |
-- Module      : Game.State
-- Description : Game state management
-- 
-- This module handles the game state, including initializing the game,
-- making moves, and managing the current state of the game.

module Game.State
    ( -- * Game State
      module Types.Game
    , initialGameState
      -- * Game Actions
    , selectPiece
    , deselectPiece
    , makeMove
    , startNewGame
    ) where

import Board.Types (Player(..), Position)
import Board.Construction (initialBoard)
import Board.Types (Piece(..), piecePlayer)
import Types.Game (GameState(..))
import Rules.Capture (makeMove)

-- | Creates the initial game state.
-- The game starts with Black player's turn and no piece selected.
initialGameState :: GameState
initialGameState = GameState initialBoard Black Nothing

-- | Start a new game with initial board setup
startNewGame :: GameState
startNewGame = initialGameState

-- | Select a piece at the given position if it belongs to the current player
-- Returns Nothing if the selection is invalid.
selectPiece :: GameState -> Position -> Maybe GameState
selectPiece gs@(GameState bd currentPl _) pos =
    case bd !! snd pos !! fst pos of
        Just piece | piecePlayer piece == currentPl -> 
            Just $ gs { selectedPiece = Just pos }
        _ -> Nothing  -- Invalid selection

-- | Deselect the currently selected piece
deselectPiece :: GameState -> GameState
deselectPiece gs = gs { selectedPiece = Nothing }
