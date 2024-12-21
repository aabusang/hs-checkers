-- |
-- Module      : Game.Mode
-- Description : Game modes and difficulty settings
-- 
-- This module defines different game modes (single player vs two player)
-- and AI difficulty levels for single player mode.

module Game.Mode
    ( GameMode(..)
    ) where

-- | Represents the game mode
data GameMode = SinglePlayer  -- ^ Play against AI
              | TwoPlayer    -- ^ Two human players
              deriving (Show, Eq)
