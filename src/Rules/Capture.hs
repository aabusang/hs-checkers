-- |
-- Module      : Rules.Capture
-- Description : Capture rules for the Checkers game
-- 
-- This module handles the rules for capturing opponent pieces,
-- including jump moves and multiple captures.

module Rules.Capture
    ( canJumpAgain
    , makeMove
    ) where

import Board.Types (Player(..), Position, Board)
import Types.Game (GameState(..))
import Rules.Movement (movePiece, getValidJumpMoves)
import Rules.Promotion (promoteKings)

-- | Gets the opposite player
-- Examples:
--   switchPlayer Black = White
--   switchPlayer White = Black
switchPlayer :: Player -> Player
switchPlayer Black = White
switchPlayer White = Black

-- | Check if a piece can jump again
-- Checks if the given board and position allow another jump
canJumpAgain :: Board  -- ^ The current board state
             -> Position  -- ^ The position to check for jumps
             -> Player  -- ^ The current player
             -> Bool
canJumpAgain inputBoard inputPos inputPlayer =
    not $ null $ getValidJumpMoves inputBoard inputPos inputPlayer

-- | Make a move and update the game state
-- Handles moving a piece, potential promotions, and player turns
makeMove :: GameState  -- ^ The current game state
         -> Position   -- ^ The starting position of the move
         -> Position   -- ^ The destination position of the move
         -> GameState  -- ^ The updated game state after the move
makeMove (GameState inputBoard inputPlayer _) fromPos toPos =
    let updatedBoard = movePiece inputBoard fromPos toPos
        boardWithPromotions = promoteKings updatedBoard
        canJumpMore = canJumpAgain boardWithPromotions toPos inputPlayer
        nextPlayer = if canJumpMore 
                    then inputPlayer  
                    else switchPlayer inputPlayer  
        newSelectedPiece = if canJumpMore
                          then Just toPos    
                          else Nothing    
    in GameState boardWithPromotions nextPlayer newSelectedPiece
