-- |
-- Module      : Rules.Movement
-- Description : Movement rules for the Checkers game
-- 
-- This module handles the basic movement rules for checker pieces,
-- including direction validation and move generation.

module Rules.Movement
    ( -- * Movement Validation
      isValidMove
    , isValidDirection
    , getValidMoves
    , getAllPossibleMoves
    , updateBoard
    , getValidBasicMoves
    ) where

import Board.Types (Player(..), PieceType(..), Piece(..), Board)
import Board.Validation (isValidBoardPosition, isEmpty)
import Board.Operations (getPieceAt, movePiece)
import Rules.Jumps (getAllValidJumpMoves)
import Rules.Capture (isCaptureMove)
import Types.Common (Position)
import Types.Game (GameState(..))


-- _________________________________ DIRECTION VALIDATION _________________________________

-- | Get the movement direction for a player
-- White moves up (-1), Black moves down (+1)
playerDirection :: Player -> Int
playerDirection White = -1
playerDirection Black = 1

-- | Check if a move is in the correct direction for a piece
isValidDirection :: Piece -> Position -> Position -> Bool
isValidDirection (Piece player pisType) (fromRow, _) (toRow, _) =
    case pisType of
        King -> True  -- Kings can move in any direction
        Man  -> case player of
            White -> toRow < fromRow  -- White moves up (decreasing row)
            Black -> toRow > fromRow  -- Black moves down (increasing row)


-- _________________________________ MOVE VALIDATION _________________________________

-- | Check if a move is valid according to the game rules
isValidMove :: GameState -> Position -> Position -> Bool
isValidMove (GameState gameBoard player _ _ _ _) fromPos toPos =
    case getPieceAt gameBoard fromPos of
        Just piece -> isValidPieceMove piece gameBoard player fromPos toPos
        Nothing -> False

-- | Check if a piece's move is valid
isValidPieceMove :: Piece -> Board -> Player -> Position -> Position -> Bool
isValidPieceMove piece gameBoard player fromPos toPos =
    pieceOwner piece == player                   -- Must be current player's piece
    && isValidBoardPosition toPos                -- Must be within board
    && isEmpty gameBoard toPos                   -- Destination must be empty
    && isValidDirection piece fromPos toPos      -- Must move in valid direction
    && (isBasicMove fromPos toPos           -- Must be either basic move
        || isCaptureMove fromPos toPos)      -- or capture move

-- | Check if two positions represent a basic move (one square diagonally)
isBasicMove :: Position -> Position -> Bool
isBasicMove (fromRow, fromCol) (toRow, toCol) =
    abs (fromRow - toRow) == 1 && abs (fromCol - toCol) == 1


-- _________________________________ MOVE GENERATION _________________________________

-- | Get possible moves (diagonally forward) for a Man piece
getManMoves :: Position -> Player -> [Position]
getManMoves (row, col) player =
    let dir   = playerDirection player
        moves = [(row + dir, col - 1), (row + dir, col + 1)] 
    in filter isValidBoardPosition moves

-- | Get possible moves for a King piece
getKingMoves :: Position -> [Position]
getKingMoves (row, col) =
    let moves = [ (row + 1, col + 1)  -- down-right
                , (row + 1, col - 1)  -- down-left
                , (row - 1, col + 1)  -- up-right
                , (row - 1, col - 1)  -- up-left
                ]
    in filter isValidBoardPosition moves

-- | Get all valid basic moves for a piece at a given position
getValidBasicMoves :: Piece -> Position -> [Position]
getValidBasicMoves (Piece player pisType) pos =
    case pisType of
        Man -> getManMoves pos player
        King -> getKingMoves pos

-- | Get all valid moves for a piece at a position
getValidMoves :: GameState -> Position -> [Position]
getValidMoves (GameState gameBoard player _ _ _ _) pos =
    case getPieceAt gameBoard pos of
        Just piece@(Piece pisPlayer _) | pisPlayer == player -> 
            getValidBasicMoves piece pos ++ getAllValidJumpMoves gameBoard pos piece
        _ -> []  -- No piece or opponent's piece

-- | Get all possible moves for all pieces of the current player
getAllPossibleMoves :: GameState -> [(Position, [Position])]
getAllPossibleMoves gameState@(GameState gameBoard thisPlayer _ _ _ _) =
    let positions = [(row, col) | row <- [0..7], col <- [0..7]]
        playerPieces = filter (isPieceOfPlayer gameBoard thisPlayer) positions
        validMovesPos = [(pos, moves) | pos <- playerPieces,
                                   let moves = getValidMoves gameState pos,
                                   not (null moves)]
    in validMovesPos

-- | Check if a position contains a piece belonging to the given player
isPieceOfPlayer :: Board -> Player -> Position -> Bool
isPieceOfPlayer gameBoard player pos =
    case getPieceAt gameBoard pos of
        Just (Piece owner _) -> owner == player
        Nothing -> False


-- _________________________________ BOARD UPDATES _________________________________

-- | Update the board after a move
updateBoard :: Board -> Position -> Position -> Board
updateBoard = movePiece  -- This is just an alias for movePiece from Board.Operations
