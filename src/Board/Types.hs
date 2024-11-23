-- |
-- Module      : Board.Types
-- Description : Core types for the checker board
-- 
-- This module contains the fundamental types needed to represent a checker board,
-- its pieces, and their positions.

module Board.Types
    ( -- * Game Pieces
      Player(..)
    , PieceType(..)
    , Piece(..)
      -- * Board Types
    , Position
    , Board
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
