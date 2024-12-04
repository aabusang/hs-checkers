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

-- | Checks if a piece should be promoted to king based on its position
-- Black pieces are promoted when they reach row 7 (bottom)
-- White pieces are promoted when they reach row 0 (top)
-- Examples:
--   shouldPromoteToKing Black (3, 7) = True   -- Black piece at bottom row
--   shouldPromoteToKing White (4, 0) = True   -- White piece at top row
--   shouldPromoteToKing Black (2, 5) = False  -- Black piece not at bottom
shouldPromoteToKing :: Player -> Position -> Bool
shouldPromoteToKing Black (_, y) = y == 7  -- Black pieces promote at bottom row
shouldPromoteToKing White (_, y) = y == 0  -- White pieces promote at top row

-- | Promotes all eligible pieces to kings
-- This function:
-- 1. Checks each position on the board
-- 2. If there's a piece that should be promoted, promotes it to a king
-- 3. Leaves all other pieces unchanged
promoteKings :: Board -> Board
promoteKings currentBoard =
    -- Process each row with its y-coordinate
    [[promotePieceIfNeeded (x, y) piece 
      | (x, piece) <- zip [0..] row]  -- Process each piece with its x-coordinate
     | (y, row) <- zip [0..] currentBoard]   -- Process each row with its y-coordinate
  where
    -- Helper function to promote a single piece if needed
    promotePieceIfNeeded :: Position -> Maybe Piece -> Maybe Piece
    promotePieceIfNeeded pos (Just (Piece player Man))
        | shouldPromoteToKing player pos = Just (Piece player King)
    promotePieceIfNeeded _ piece = piece  -- Keep the piece unchanged if no promotion needed
