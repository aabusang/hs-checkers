-- |
-- Module      : Rules.GameEnd
-- Description : Game end conditions for Checkers
-- 
-- This module handles checking for game end conditions,
-- such as when a player has no valid moves remaining.

module Rules.GameEnd
    ( isGameOver
    , getWinner
    ) where

import Board.Types (Player(..))
import Types.Game (GameState(..))  
import Rules.Movement (getValidMoves)

-- | Determine if the game is over by checking if the current player
-- has any possible moves left on the board.
-- 
-- A game is considered over when no pieces can be moved.
-- 
-- Example:
--   isGameOver initialGameState  -- False
--   isGameOver finalGameState    -- True
isGameOver :: GameState -> Bool
isGameOver gameState = 
    -- Create all board positions
    let boardPositions = [(x, y) | x <- [0..7], y <- [0..7]]
        
        -- Get valid moves for each board position
        validMovesPerPosition = map (getValidMoves gameState) boardPositions
    
    -- If ALL positions have NO valid moves, the game is over
    in all null validMovesPerPosition

-- | Determine the winner of the game.
-- 
-- Returns:
--   * 'Just' the winning player if the game is over
--   * 'Nothing' if the game is still in progress
-- 
-- The winner is the opposite of the current player 
-- when no moves are possible.
-- 
-- Example:
--   getWinner finalGameState  -- Just White
--   getWinner initialGameState  -- Nothing
getWinner :: GameState -> Maybe Player
getWinner gameState
    | isGameOver gameState = Just $ switchPlayer (extractCurrentPlayer gameState)
    | otherwise            = Nothing
  where
    -- Extract current player from game state
    extractCurrentPlayer :: GameState -> Player
    extractCurrentPlayer (GameState _ player _) = player

    -- Switch the current player (determines the winner)
    switchPlayer :: Player -> Player
    switchPlayer Black = White
    switchPlayer White = Black
