-- |
-- Module      : Types.Game
-- Description : Core game state type
-- 
-- This module contains the GameState type which represents the current state
-- of a checkers game.

module Types.Game
    ( GameState(..)
    , GameStatus(..)
    , initialGameState
    ) where

import Board.Types (Board, Player(..))
import Types.Common (Position)
import Board.Construction (initialBoard)

-- | Game status indicating whether the game is ongoing, won, or drawn
data GameStatus = 
    Ongoing
    | Won Player  -- ^ Game won by specified player
    | Draw        -- ^ Game ended in a draw
    deriving (Show, Eq)

-- | Represents the current state of the game
data GameState = GameState
    { board             :: Board   -- ^ The current board configuration
    , currentPlayer     :: Player  -- ^ The player whose turn it is
    , selectedPiecePos  :: Maybe Position  -- ^ The currently selected piece's position
    , validMoves        :: [Position]  -- ^ List of valid moves for the selected piece
    , gameStatus        :: GameStatus -- ^ Current status of the game
    , movesSinceCapture :: Int -- ^ Moves made since last capture (for draw detection)
    } deriving (Show)

-- | Initial game state with an empty board and Black as the starting player
initialGameState :: GameState
initialGameState = GameState
    { board             = initialBoard
    , currentPlayer     = Black
    , selectedPiecePos  = Nothing
    , validMoves        = []
    , gameStatus        = Ongoing
    , movesSinceCapture = 0
    }
