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
    , getMultiCaptures
    ) where

import Board.Types (Player(..), Board, Piece(..), PieceType(..))
import Board.Operations (getPieceAt)
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

-- | Get all possible single capture moves from a position
getPossibleCaptures :: Board -> Position -> Player -> [Position]
getPossibleCaptures board fromPos@(row, col) player =
    case getPieceAt board fromPos of
        Nothing -> []
        Just (Piece _ _) ->  
            let possibleCaptures = [ (row + 2, col + 2)  
                                 , (row + 2, col - 2)  
                                 , (row - 2, col + 2)  
                                 , (row - 2, col - 2)  
                                 ]
            in filter (isValidCapture board fromPos player) possibleCaptures

-- | Check if a capture move is valid
isValidCapture :: Board -> Position -> Player -> Position -> Bool
isValidCapture board fromPos player toPos =
    isValidBoardPosition toPos 
    && isEmpty board toPos  
    && isOpponentPiece board (getCapturedPosition fromPos toPos) player

-- | Get all possible multi-capture sequences from a position
-- Returns a list of capture sequences, where each sequence is a list of positions
getMultiCaptures :: Board -> Position -> Player -> [[Position]]
getMultiCaptures board fromPos player = 
    let initialCaptures = getPossibleCaptures board fromPos player
    in if null initialCaptures
       then []
       else map (\capture -> fromPos : findCaptureSequence board capture player [fromPos]) initialCaptures

-- | Helper function to recursively find capture sequences
-- Returns the sequence of positions that form a valid capture sequence
findCaptureSequence :: Board -> Position -> Player -> [Position] -> [Position]
findCaptureSequence board currentPos player visited =
    let nextCaptures = filter (\pos -> pos `notElem` visited) $ 
                      getPossibleCaptures board currentPos player
    in if null nextCaptures
       then [currentPos]
       else currentPos : concatMap (\nextPos -> findCaptureSequence board nextPos player (currentPos:visited)) nextCaptures
