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
import Game.State (GameState(..))
import Rules.Movement (getValidMoves)

-- | Checks if the game is over
-- A game is over when the current player has no valid moves remaining
isGameOver :: GameState -> Bool
isGameOver gs =
    all (null . getValidMoves gs) [(x, y) | x <- [0..7], y <- [0..7]]

-- | Gets the winner of the game, if any
getWinner :: GameState -> Maybe Player
getWinner gs@(GameState _ player _)
    | isGameOver gs = Just (oppositePlayer player)
    | otherwise = Nothing
  where
    oppositePlayer :: Player -> Player
    oppositePlayer Black = White
    oppositePlayer White = Black
