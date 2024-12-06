-- |
-- Module      : Rules.Jumps
-- Description : Jump move rules for the Checkers game
-- 
-- This module handles all jump-related rules and calculations,
-- including single jumps and multiple jump sequences.

module Rules.Jumps
    ( getAllValidJumpMoves
    , getManJumpMoves
    , getKingJumpMoves
    , isValidJumpMove
    , getMiddlePosition
    ) where

import Board.Types (Board, Player(..), Piece(..), PieceType(..))
import Board.Validation (isEmpty, isOpponentPiece, isValidBoardPosition)
import Types.Common (Position)

-- | Get the position between two positions (for jump moves)
getMiddlePosition :: Position -> Position -> Position
getMiddlePosition (fromRow, fromCol) (toRow, toCol) = 
    ((fromRow + toRow) `div` 2, (fromCol + toCol) `div` 2)

-- | Get possible jump positions for a Man piece
getManJumpMoves :: Position -> Player -> [Position]
getManJumpMoves (row, col) player =
    let dir = if player == White then 1 else -1
        -- Man can jump diagonally forward
        jumps = [(row + dir*2, col - 2), (row + dir*2, col + 2)]
    in filter isValidBoardPosition jumps

-- | Get possible jump positions for a King piece
getKingJumpMoves :: Position -> [Position]
getKingJumpMoves (row, col) =
    let jumps = [ (row + 2, col + 2)  -- down-right
                , (row + 2, col - 2)  -- down-left
                , (row - 2, col + 2)  -- up-right
                , (row - 2, col - 2)  -- up-left
                ]
    in filter isValidBoardPosition jumps

-- | Check if a jump move is valid
isValidJumpMove :: Board -> Position -> Position -> Player -> Bool
isValidJumpMove board fromPos toPos player =
    let middlePos = getMiddlePosition fromPos toPos
    in isValidBoardPosition toPos 
       && isEmpty board toPos  -- landing spot must be empty
       && isOpponentPiece board middlePos player  -- must jump over opponent's piece

-- | Gets all valid jump moves for a piece
getAllValidJumpMoves :: Board -> Position -> Piece -> [Position]
getAllValidJumpMoves board pos (Piece player pisType) =
    let possibleJumps = case pisType of
            Man -> getManJumpMoves pos player
            King -> getKingJumpMoves pos
    in filter (\toPos -> isValidJumpMove board pos toPos player) possibleJumps
