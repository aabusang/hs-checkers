-- |
-- Module      : Board.Operations
-- Description : Basic board operations for the Checkers game
-- 
-- This module provides basic operations for interacting with the game board,
-- such as retrieving pieces and checking board positions.

module Board.Operations
    ( -- * Board Access
      getPieceAt
    , setPosition
    , movePiece
    ) where

import Board.Types (Board, Piece)
import Types.Common (Position)

-- | Get a piece from the board at a given position
getPieceAt :: Board -> Position -> Maybe Piece
getPieceAt board (row, col) = board !! row !! col

-- | Set a position on the board to a specific value (piece or nothing)
setPosition :: Board -> Position -> Maybe Piece -> Board
setPosition board (row, col) value =
    let rowList = board !! row
        newRow = take col rowList ++ [value] ++ drop (col + 1) rowList
    in take row board ++ [newRow] ++ drop (row + 1) board

-- | Move a piece on the board from one position to another
movePiece :: Board -> Position -> Position -> Board
movePiece board fromPos toPos =
    let piece = getPieceAt board fromPos
        -- First set the piece at new position
        boardWithNewPos = setPosition board toPos piece
        -- Then clear the old position
        finalBoard = setPosition boardWithNewPos fromPos Nothing
    in finalBoard