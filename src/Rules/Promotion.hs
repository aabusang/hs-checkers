-- |
-- Module      : Rules.Promotion
-- Description : Promotion rules for checker pieces
-- 
-- This module handles the rules for promoting checker pieces to kings
-- when they reach the opposite end of the board.

module Rules.Promotion
    ( shouldPromoteToKing
    , promoteKings
    ) where

import Board.Types (Player(..), PieceType(..), Piece(..), Position, Board)

-- | Checks if a piece should be promoted to king
-- A piece is promoted when it reaches the opposite end of the board
shouldPromoteToKing :: Player -> Position -> Bool
shouldPromoteToKing Black (_, y) = y == 7  -- Black pieces promote at row 7
shouldPromoteToKing White (_, y) = y == 0  -- White pieces promote at row 0

-- | Promotes pieces to kings if they reach the opposite end of the board
promoteKings :: Board -> Board
promoteKings boardState =
  [[case piece of
      Just (Piece Black Man) | shouldPromoteToKing Black (x, y) -> Just (Piece Black King)
      Just (Piece White Man) | shouldPromoteToKing White (x, y) -> Just (Piece White King)
      _ -> piece
    | (x, piece) <- zip [0..] row]
   | (y, row) <- zip [0..] boardState]
