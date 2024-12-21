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
    , getManCaptures
    , getKingCaptures
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

-- | Get all possible single capture moves from a position for a regular piece (man)
getManCaptures :: Board -> Position -> Player -> [Position]
getManCaptures board fromPos@(row, col) player =
    let captureDirections = [(2, 2), (2, -2), (-2, 2), (-2, -2)]  -- All possible capture jumps
        possibleCaptures = map (\(dRow, dCol) -> (row + dRow, col + dCol)) captureDirections
        isValidManCapture toPos = 
            isValidBoardPosition toPos 
            && isEmpty board toPos  
            && isOpponentPiece board (getCapturedPosition fromPos toPos) player
    in filter isValidManCapture possibleCaptures

-- | Get all possible single capture moves from a position for a king piece
getKingCaptures :: Board -> Position -> Player -> [Position]
getKingCaptures board (row, col) player =
    let directions = [(1, 1), (1, -1), (-1, 1), (-1, -1)]  -- All diagonal directions
        -- For each direction, look for opponent pieces and possible jumps
        capturesInDirection (dRow, dCol) = 
            -- Look at each position in this direction until we hit something or the board edge
            let potentialJumps = [(row + i*dRow, col + i*dCol, row + (i+1)*dRow, col + (i+1)*dCol) 
                                | i <- [1..6]]  -- One less than board size to allow for landing
                validJumps = takeWhile (\(r1,c1,r2,c2) -> 
                    isValidBoardPosition (r1,c1) && isValidBoardPosition (r2,c2)) potentialJumps
                -- Find the first opponent piece and the empty space after it
                findJump [] = Nothing
                findJump ((r1,c1,r2,c2):rest) = 
                    if isOpponentPiece board (r1,c1) player && isEmpty board (r2,c2)
                    then Just (r2,c2)  -- Return the landing position
                    else if isEmpty board (r1,c1)
                         then findJump rest  -- Keep looking if square is empty
                         else Nothing  -- Stop if we hit any other piece
            in maybe [] (:[]) (findJump validJumps)
    in concatMap capturesInDirection directions

-- | Get all possible captures for a piece (either man or king)
getPossibleCaptures :: Board -> Position -> Player -> [Position]
getPossibleCaptures board pos player =
    case getPieceAt board pos of
        Nothing -> []
        Just (Piece _ pisType) -> 
            case pisType of
                Man -> getManCaptures board pos player
                King -> getKingCaptures board pos player

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
