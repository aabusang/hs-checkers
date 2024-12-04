-- |
-- Module      : Game.Mode
-- Description : Game modes and difficulty settings
-- 
-- This module defines different game modes (single player vs two player)
-- and AI difficulty levels for single player mode.

module Game.Mode
    ( GameMode(..)
    , Difficulty(..)
    ) where

-- | Represents the AI difficulty levels
data Difficulty = Easy    -- ^ AI makes random valid moves
                | Medium  -- ^ AI looks ahead 1-2 moves
                | Hard    -- ^ AI looks ahead 2-3 moves and uses better evaluation
                deriving (Show, Eq)

-- | Represents the game mode
data GameMode = SinglePlayer Difficulty  -- ^ Play against AI with specified difficulty
              | TwoPlayer               -- ^ Two human players
              deriving (Show, Eq)
