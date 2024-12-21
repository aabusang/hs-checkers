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
    , isCaptureMove
    , getCapturedPosition
    , getPossibleCaptures
    ) where

import Board.Types (Player(..), PieceType(..), Piece(..), Board)
import Board.Validation (isValidBoardPosition, isEmpty, isOpponentPiece)
import Board.Operations (getPieceAt, movePiece)
import Rules.Capture (isCaptureMove, getCapturedPosition, getPossibleCaptures, getMultiCaptures)
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
isValidDirection (Piece player pisType) fromPos toPos =
    case pisType of
        King -> True  -- Kings can move in any direction
        Man  -> isCaptureMove fromPos toPos  -- Allow any direction for captures
                || case player of  -- Normal moves follow direction rules
                    White -> toRow < fromRow  -- White moves up (decreasing row)
                    Black -> toRow > fromRow  -- Black moves down (increasing row)
    where (fromRow, _) = fromPos
          (toRow, _) = toPos


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
    && (isBasicMove fromPos toPos               -- Must be either basic move
        || (isCaptureMove fromPos toPos         -- or a valid capture move
            && isOpponentPiece gameBoard (getCapturedPosition fromPos toPos) player))  -- with opponent's piece

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
        Nothing -> []
        Just piece@(Piece owner _) ->
            if owner /= player
            then []
            else
                let basicMoves = getValidBasicMoves piece pos
                    -- Get all possible capture sequences
                    captureSequences = getMultiCaptures gameBoard pos player
                    -- Take the next position from each capture sequence
                    captureMoves = case captureSequences of
                        [] -> []
                        seqs -> map (head . tail) seqs  -- Get the first move of each sequence
                in
                    -- If captures are available, they are mandatory
                    if not (null captureMoves)
                    then captureMoves
                    else filter (isEmpty gameBoard) basicMoves

-- | Get all possible moves for all pieces of the current player
getAllPossibleMoves :: GameState -> [(Position, [Position])]
getAllPossibleMoves gameState@(GameState board currentPlayer _ _ _ _) =
    let allPositions = [(r,c) | r <- [0..7], c <- [0..7]]
        playerPositions = filter (isPieceOfPlayer board currentPlayer) allPositions
    in map (\pos -> (pos, getValidMoves gameState pos)) playerPositions

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
