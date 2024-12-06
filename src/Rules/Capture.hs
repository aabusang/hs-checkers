-- |
-- Module      : Rules.Capture
-- Description : Capture rules for the Checkers game
-- 
-- This module handles the rules for capturing opponent pieces,
-- including jump moves and multiple captures.

module Rules.Capture
    ( -- * Capture Validation
      isCaptureMove
    , hasCaptureMoves
      -- * Capture Information
    , getCapturedPosition
    , getPossibleCaptures
    ) where

import Board.Types (Player(..), Board)
import Board.Validation (isValidBoardPosition, isEmpty, isOpponentPiece)
import Types.Common (Position)


-- _________________________________ CAPTURE VALIDATION _________________________________

-- | Check if a move is a capture move (jumping over an opponent's piece)
isCaptureMove :: Position -> Position -> Bool
isCaptureMove (fromRow, fromCol) (toRow, toCol) = 
    abs (fromRow - toRow) == 2 && abs (fromCol - toCol) == 2

-- | Check if a player has any possible capture moves from a position
hasCaptureMoves :: Board -> Position -> Player -> Bool
hasCaptureMoves board pos player = 
    not (null (getPossibleCaptures board pos player))


-- _________________________________ CAPTURE INFORMATION _________________________________

-- | Get the position of the piece being captured in a jump move
getCapturedPosition :: Position -> Position -> Position
getCapturedPosition fromPos toPos = 
    let (fromRow, fromCol) = fromPos
        (toRow, toCol) = toPos
        capturedRow = (fromRow + toRow) `div` 2
        capturedCol = (fromCol + toCol) `div` 2
    in (capturedRow, capturedCol)

-- | Get all possible capture moves from a position
getPossibleCaptures :: Board -> Position -> Player -> [Position]
getPossibleCaptures board fromPos@(row, col) player =
    let possibleCaptures = [ (row + 2, col + 2)  -- down-right
                          ,  (row + 2, col - 2)  -- down-left
                          ,  (row - 2, col + 2)  -- up-right
                          ,  (row - 2, col - 2)  -- up-left
                          ]
    in filter (isValidCapture board fromPos player) possibleCaptures

-- | Check if a capture move is valid
isValidCapture :: Board -> Position -> Player -> Position -> Bool
isValidCapture board fromPos player toPos =
    isValidBoardPosition toPos 
    && isEmpty board toPos  -- landing spot must be empty
    && isOpponentPiece board (getCapturedPosition fromPos toPos) player
