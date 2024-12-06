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
isValidBoardPosition (x, y) = x >= 0 && x < 8 && y >= 0 && y < 8

-- | Check if a position is empty
isEmpty :: Board -> Position -> Bool
isEmpty board pos = 
    case board !! fst pos !! snd pos of
        Nothing -> True
        _ -> False


-- | Check if a position contains an opponent's piece
isOpponentPiece :: Board -> Position -> Player -> Bool
isOpponentPiece board pos thisPlayer =
    case board !! fst pos !! snd pos of
        Just (Piece owner _) -> owner /= thisPlayer
        Nothing -> False
