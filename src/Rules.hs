{-|
Module      : Rules
Description : Game rules for the Checkers game
Copyright   : (c) Your Name, 2024
License     : Your License

This module contains all the rules and validation logic for the Checkers game.
It defines what moves are legal, when pieces can be promoted, and when the game is over.
-}
module Rules
    ( -- * Move Validation
      isValidMove
    , isValidDirection
      -- * Game Rules
    , canJumpAgain
    , isGameOver
    , makeMove
      -- * Board Rules
    , shouldPromoteToKing
    , isWithinBoard
    , getValidMoves
    , oppositePlayer
    ) where

import Types (Player(..), PieceType(..), Piece(..), Position, Board, GameState(..))
import Data.Maybe (fromJust)

-- | Gets the opposite player
oppositePlayer :: Player -> Player
oppositePlayer Black = White
oppositePlayer White = Black

-- | Checks if a position is within the board boundaries (8x8)
isWithinBoard :: Position -> Bool
isWithinBoard (x, y) = x >= 0 && x < 8 && y >= 0 && y < 8

-- | Checks if a piece should be promoted to king
-- A piece is promoted when it reaches the opposite end of the board
shouldPromoteToKing :: Player -> Position -> Bool
shouldPromoteToKing Black (_, y) = y == 7  -- Black pieces promote at row 7
shouldPromoteToKing White (_, y) = y == 0  -- White pieces promote at row 0

-- | Checks if a move follows the piece's movement rules
isValidDirection :: Piece -> Position -> Position -> Bool
isValidDirection (Piece player pieceType) (fromX, fromY) (toX, toY) =
    let dx = toX - fromX
        dy = toY - fromY
        direction = if player == Black then 1 else -1
        validDirections = case pieceType of
            Man -> [direction]  -- Men can only move forward
            King -> [1, -1]    -- Kings can move both directions
    in abs dx == abs dy  -- Must move diagonally
       && (abs dx == 1 || abs dx == 2)  -- Must move 1 square (normal) or 2 squares (jump)
       && dy `div` abs dy `elem` validDirections  -- Must move in valid direction

-- | Checks if a move from one position to another is valid according to the rules
isValidMove :: GameState -> Position -> Position -> Bool
isValidMove (GameState board player _) from@(fromX, fromY) to@(toX, toY) =
    let mPiece = if isWithinBoard from 
                 then board !! fromY !! fromX
                 else Nothing
    in case mPiece of
        Nothing -> False  -- No piece at starting position
        Just piece@(Piece p _) ->
            p == player &&                -- Must be current player's piece
            isWithinBoard to &&           -- Must stay on board
            null (board !! toY !! toX) && -- Destination must be empty
            isValidDirection piece from to && -- Must move in valid direction
            (if abs (toX - fromX) == 2    -- If jumping
             then case board !! ((fromY + toY) `div` 2) !! ((fromX + toX) `div` 2) of
                    Just (Piece p' _) -> p' /= player  -- Must jump over opponent's piece
                    _ -> False
             else True)                   -- Normal moves are always valid if other conditions met

-- | Makes a move from one position to another.
-- If the move is valid:
--   1. Updates the board with the new piece position
--   2. Removes any captured pieces
--   3. Promotes pieces to kings if they reach the opposite side
--   4. Updates the current player
-- If the move is not valid, returns the original game state.
makeMove :: GameState -> Position -> Position -> GameState
makeMove gs@(GameState board player _) from@(fromX, fromY) to@(toX, toY) =
  if isValidMove gs from to
  then let newBoard = movePiece board from to
           promotedBoard = promoteKings newBoard
           hasMoreJumps = canJumpAgain promotedBoard to player
           newPlayer = if hasMoreJumps then player else oppositePlayer player
           newSelected = if hasMoreJumps then Just to else Nothing
       in GameState promotedBoard newPlayer newSelected
  else gs

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

-- | Updates a single position on the board with a new piece
updateBoard :: Board -> Position -> Maybe Piece -> Board
updateBoard boardState (x, y) newPiece =
  take y boardState ++
  [take x (boardState !! y) ++ [newPiece] ++ drop (x + 1) (boardState !! y)] ++
  drop (y + 1) boardState

-- | Promotes pieces to kings if they reach the opposite end of the board
promoteKings :: Board -> Board
promoteKings boardState =
  [[case piece of
      Just (Piece Black Man) | shouldPromoteToKing Black (x, y) -> Just (Piece Black King)
      Just (Piece White Man) | shouldPromoteToKing White (x, y) -> Just (Piece White King)
      _ -> piece
    | (x, piece) <- zip [0..] row]
   | (y, row) <- zip [0..] boardState]

-- | Checks if a player can jump again after making a move
canJumpAgain :: Board -> Position -> Player -> Bool
canJumpAgain board pos player =
    let moves = getValidJumpMoves board pos player
    in not (null moves)

-- | Gets all valid jump moves for a piece
getValidJumpMoves :: Board -> Position -> Player -> [Position]
getValidJumpMoves board (x, y) player =
    case board !! y !! x of
        Just piece@(Piece p _) | p == player ->
            [(x + 2*dx, y + 2*dy) | 
             (dx, dy) <- [(1,1), (1,-1), (-1,1), (-1,-1)],
             let jumpTo = (x + 2*dx, y + 2*dy),
             isWithinBoard jumpTo,
             isValidJump board piece (x, y) jumpTo]
        _ -> []

-- | Checks if a jump move is valid
isValidJump :: Board -> Piece -> Position -> Position -> Bool
isValidJump board piece from@(fromX, fromY) to@(toX, toY) =
    isWithinBoard to &&
    null (board !! toY !! toX) &&  -- Destination must be empty
    isValidDirection piece from to &&
    case board !! ((fromY + toY) `div` 2) !! ((fromX + toX) `div` 2) of
        Just (Piece p _) -> p /= piecePlayer piece  -- Must jump over opponent's piece
        _ -> False
  where
    piecePlayer (Piece p _) = p

-- | Checks if the game is over
isGameOver :: GameState -> Bool
isGameOver gs =
    all (null . getValidMoves gs) [(x, y) | x <- [0..7], y <- [0..7]]

-- | Gets all valid moves for a piece at a given position
getValidMoves :: GameState -> Position -> [Position]
getValidMoves gs@(GameState board player _) pos@(x, y) =
    case board !! y !! x of
        Just (Piece p _) | p == player ->
            let normalMoves = [(x + dx, y + dy) | 
                             (dx, dy) <- [(1,1), (1,-1), (-1,1), (-1,-1)],
                             isValidMove gs pos (x + dx, y + dy)]
                jumpMoves = getValidJumpMoves board pos player
            in if not (null jumpMoves)
               then jumpMoves  -- If jumps are available, they are mandatory
               else normalMoves
        _ -> []
