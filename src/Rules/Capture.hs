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
oppositePlayer :: Player -> Player
oppositePlayer Black = White
oppositePlayer White = Black

-- | Checks if a player can jump again after making a move
canJumpAgain :: Board -> Position -> Player -> Bool
canJumpAgain b' pos player' =
    not . null $ getValidJumpMoves b' pos player'

-- | Makes a move from one position to another.
-- If the move is valid:
--   1. Updates the board with the new piece position
--   2. Removes any captured pieces
--   3. Promotes pieces to kings if they reach the opposite side
--   4. Updates the current player
-- If the move is not valid, returns the original game state.
makeMove :: GameState -> Position -> Position -> GameState
makeMove (GameState b player _) from to =
    let newBoard = movePiece b from to
        promotedBoard = promoteKings newBoard
        hasMoreJumps = canJumpAgain promotedBoard to player
        newPlayer = if hasMoreJumps then player else oppositePlayer player
        newSelected = if hasMoreJumps then Just to else Nothing
    in GameState promotedBoard newPlayer newSelected
