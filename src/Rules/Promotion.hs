-- |
-- Module      : Rules.Promotion
-- Description : Promotion rules for the Checkers game
-- 
-- This module handles the rules for promoting checker pieces to kings
-- when they reach the opposite end of the board.


module Rules.Promotion
    ( -- * Promotion Checks
      canBePromoted
    , getPromotionRow
      -- * Piece Promotion
    , promoteToKing
    ) where

import Board.Types (Player(..), PieceType(..), Piece(..))
import Types.Common (Position)


-- _________________________________ PROMOTION CHECKS _________________________________

-- | Get the row where a player's pieces get promoted
-- Black pieces promote at row 0 (top), White pieces at row 7 (bottom)
getPromotionRow :: Player -> Int
getPromotionRow Black = 0  -- Black promotes at top row
getPromotionRow White = 7  -- White promotes at bottom row

-- | Check if a piece can be promoted based on its position
-- For example:
-- > canBePromoted Black (0, 3)  -- Returns True (Black piece at top row)
-- > canBePromoted White (7, 2)  -- Returns True (White piece at bottom row)
-- > canBePromoted Black (6, 1)  -- Returns False (Black piece not at top)
canBePromoted :: Player -> Position -> Bool
canBePromoted player (row, _) = 
    -- We only care about the row for promotion
    -- Black promotes at row 0 (top)
    -- White promotes at row 7 (bottom)
    row == getPromotionRow player


-- _________________________________ PIECE PROMOTION _________________________________

-- | Create a king piece from a regular piece
promoteToKing :: Piece -> Piece
promoteToKing (Piece player _) = Piece player King
