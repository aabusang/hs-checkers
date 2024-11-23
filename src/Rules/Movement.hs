-- |
-- Module      : Rules.Movement
-- Description : Movement rules for the Checkers game
-- 
-- This module handles the basic movement rules for checker pieces,
-- including direction validation and board boundary checks.

module Rules.Movement
    ( -- * Movement Validation
      isValidMove
    , isValidDirection
    , getValidJumpMoves
    , isWithinBoard
    , getValidMoves
    , updateBoard
    , movePiece
    ) where

import Board.Types (Player(..), PieceType(..), Piece(..), Position, Board)
import Types.Game (GameState(..))
import Data.Maybe (fromJust)

-- | Checks if a position is within the board boundaries (8x8)
isWithinBoard :: Position -> Bool
isWithinBoard (x, y) = x >= 0 && x < 8 && y >= 0 && y < 8

-- | Checks if a move follows the piece's movement rules
isValidDirection :: Piece -> Position -> Position -> Bool
isValidDirection (Piece player pt) (fromX, fromY) (toX, toY) =
    let dy = toY - fromY
        dx = abs (toX - fromX)
        validDirection = case player of
            Black -> dy > 0  -- Black moves down
            White -> dy < 0  -- White moves up
    in case pt of
        King -> dx == abs dy        -- Kings can move diagonally in any direction
        Man -> validDirection && dx == abs dy  -- Men must move diagonally in their direction

-- | Checks if a move from one position to another is valid according to the rules
isValidMove :: GameState -> Position -> Position -> Bool
isValidMove (GameState b player _) from@(fromX, fromY) to@(toX, toY) =
    case b !! fromY !! fromX of
        Nothing -> False
        Just piece -> 
            piecePlayer piece == player && 
            isWithinBoard to && 
            null (b !! toY !! toX) && 
            isValidDirection piece from to

-- | Updates a single position on the board with a new piece
updateBoard :: Board -> Position -> Maybe Piece -> Board
updateBoard boardState (x, y) newPiece =
  take y boardState ++
  [take x (boardState !! y) ++ [newPiece] ++ drop (x + 1) (boardState !! y)] ++
  drop (y + 1) boardState

-- | Moves a piece from one position to another on the board.
-- If it's a jump move, removes the captured piece.
movePiece :: Board -> Position -> Position -> Board
movePiece boardState (fromX, fromY) (toX, toY) =
  let piece = fromJust (boardState !! fromY !! fromX)
      newBoard = updateBoard boardState (fromX, fromY) Nothing
      finalBoard = updateBoard newBoard (toX, toY) (Just piece)
  in if abs (fromX - toX) == 2
     then updateBoard finalBoard ((fromX + toX) `div` 2, (fromY + toY) `div` 2) Nothing
     else finalBoard

-- | Gets all valid moves for a piece at a given position
getValidMoves :: GameState -> Position -> [Position]
getValidMoves gs@(GameState b' player' _) pos@(x, y) =
    case b' !! y !! x of
        Nothing -> []
        Just piece ->
            if piecePlayer piece /= player'
                then []
                else validMoves ++ validJumps
  where
    validMoves = filter (isValidMove gs pos) possibleMoves
    validJumps = getValidJumpMoves b' pos player'
    possibleMoves = [(x+1, y+1), (x+1, y-1), (x-1, y+1), (x-1, y-1)]

-- | Gets all valid jump moves from a position for a specific player
getValidJumpMoves :: Board -> Position -> Player -> [Position]
getValidJumpMoves b' pos@(x, y) player =
    case b' !! y !! x of
        Just piece@(Piece p _) | p == player -> 
            [(x + 2 * dx, y + 2 * dy) |
             dx <- [-1, 1], 
             dy <- [-1, 1],
             let jumpPos = (x + 2 * dx, y + 2 * dy),
             isValidJump b' piece pos jumpPos]
        _ -> []  -- Return empty list if no piece or wrong player's piece

-- | Checks if a jump move is valid
isValidJump :: Board -> Piece -> Position -> Position -> Bool
isValidJump b' piece from@(fromX, fromY) to@(toX, toY) =
    isWithinBoard to &&
    null (b' !! toY !! toX) &&  -- destination must be empty
    isValidDirection piece from to &&
    case b' !! ((fromY + toY) `div` 2) !! ((fromX + toX) `div` 2) of
        Just (Piece p _) -> p /= piecePlayer piece  -- Must jump over opponent's piece
        _ -> False
