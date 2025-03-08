-- |
-- Module      : AI.Types
-- Description : AI types and difficulty levels
-- 
-- This module defines AI-related types and difficulty levels.

module AI.Types
    ( -- * AI Types
      Difficulty(..)
    , AIMove
    , MoveScore
    ) where

-- | AI difficulty levels
data Difficulty = Easy    -- ^ Makes random valid moves
                | Medium  -- ^ Looks one move ahead
                | Hard    -- ^ Looks multiple moves ahead
                deriving (Show, Eq)

-- | Represents a move with a score for evaluation
type AIMove = ((Int, Int), (Int, Int))  -- (fromPos, toPos)

-- | Score for evaluating board positions
type MoveScore = Int
