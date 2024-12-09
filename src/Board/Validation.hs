-- |
-- Module      : Board.Validation
-- Description : Board validation functions
-- 
-- This module contains all board-related validation functions,
-- including position and piece validation.

module Board.Validation
    ( isValidBoardPosition
    , isEmpty
    , isOpponentPiece
    ) where

import Board.Types (Board, Player(..), Piece(..))
import Types.Common (Position)

-- | Check if a position is within the bounds of the board (8x8)
isValidBoardPosition :: Position -> Bool
isValidBoardPosition (row, col) = row >= 0 && row < 8 && col >= 0 && col < 8

-- | Check if a position is empty
isEmpty :: Board -> Position -> Bool
isEmpty board (row, col) = 
    case board !! row !! col of
        Nothing -> True
        _ -> False


-- | Check if a position contains an opponent's piece
isOpponentPiece :: Board -> Position -> Player -> Bool
isOpponentPiece board (row, col) thisPlayer =
    case board !! row !! col of
        Just (Piece owner _) -> owner /= thisPlayer
        Nothing -> False
