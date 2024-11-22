{-|
Module      : Types
Description : Core types for the Checkers game
Copyright   : (c) Your Name, 2024
License     : Your License

This module contains all the core data types and type aliases used throughout the Checkers game.
It centralizes the type definitions to maintain consistency and avoid circular dependencies.
-}
module Types 
    ( -- * Game Pieces
      Player(..)
    , PieceType(..)
    , Piece(..)
      -- * Board Types
    , Position
    , Board
      -- * Game State
    , GameState(..)
    , board
    , currentPlayer
    , selectedPiece
      -- * Game Modes
    , GameMode(..)
    , Difficulty(..)
    ) where

-- | Represents a player in the game
data Player = Black  -- ^ The black pieces player
           | White  -- ^ The white pieces player
           deriving (Show, Eq)

-- | Represents the type of a checker piece
data PieceType = Man   -- ^ A regular checker piece that can only move forward
               | King  -- ^ A promoted piece that can move both forward and backward
               deriving (Show, Eq)

-- | Represents a checker piece on the board
data Piece = Piece 
    { piecePlayer :: Player     -- ^ The player who owns this piece
    , pieceType  :: PieceType   -- ^ The type of this piece (Man or King)
    } deriving (Show, Eq)

-- | Represents a position on the board as (x, y) coordinates
-- The origin (0,0) is at the top-left corner
type Position = (Int, Int)

-- | Represents the game board as a 2D array of optional pieces
-- Nothing represents an empty square
-- Just piece represents a square occupied by a piece
type Board = [[Maybe Piece]]

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
