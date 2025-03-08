-- |
-- Module      : AI.Player
-- Description : AI player implementation
-- 
-- This module provides the interface for the AI player to interact with the game.

module AI.Player
    ( -- * AI Player
      makeAIMove
    , getAIDifficulty
    ) where

import System.Random (StdGen, mkStdGen, newStdGen)
import Data.Time.Clock.POSIX (getPOSIXTime)

import Types.Game (GameState(..))
import Game.State (makeMove)
import AI.Types (Difficulty(..))
import AI.Engine (selectAIMove)

-- | Make an AI move based on the current game state and difficulty
makeAIMove :: Difficulty -> GameState -> IO (Maybe GameState)
makeAIMove difficulty gameState = do
    -- Create a random generator based on current time
    time <- round <$> getPOSIXTime
    let rng = mkStdGen time
    
    -- Select an AI move
    case selectAIMove rng difficulty gameState of
        Just ((fromPos, toPos), _) -> 
            -- Apply the move to the game state
            return $ makeMove gameState fromPos toPos
        Nothing -> 
            -- No valid moves available
            return Nothing

-- | Get the AI difficulty based on user preference or default
getAIDifficulty :: Maybe String -> Difficulty
getAIDifficulty (Just "easy") = Easy
getAIDifficulty (Just "medium") = Medium
getAIDifficulty (Just "hard") = Hard
getAIDifficulty _ = Medium  -- Default to Medium difficulty
