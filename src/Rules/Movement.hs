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
    , getValidMoves
    , getAllPossibleMoves
    , hasValidJumps
    , getMultipleJumpMoves
    , updateBoard
    , movePiece
    ) where

import Board.Types (Player(..), PieceType(..), Piece(..), Position, Board)
import Types.Game (GameState(..))
import Data.Maybe (fromJust)

-- | Checks if a move follows the piece's movement rules
isValidDirection :: Piece -> Position -> Position -> Bool
isValidDirection (Piece player pt) (fromX, fromY) (toX, toY) =
    let dy = toY - fromY
        dx = abs (toX - fromX)
    in case pt of
        King -> dx == abs dy  -- Kings can move diagonally in any direction
        Man -> case player of
            Black -> dy > 0 && dx == dy     -- Black men move down
            White -> dy < 0 && dx == -dy    -- White men move up

-- | Gets jump positions for a piece
getJumpPositions :: Board -> Position -> Piece -> [Position]
getJumpPositions inputBoard (x, y) (Piece player _) =
    filter isValidJumpMove possibleJumps
  where
    isValidJumpMove (jumpX, jumpY) =
        let midX = (x + jumpX) `div` 2
            midY = (y + jumpY) `div` 2
        in case (inputBoard !! y !! x, inputBoard !! midY !! midX, inputBoard !! jumpY !! jumpX) of
            (Just (Piece currPlayer _), Just (Piece opponent _), Nothing) ->
                currPlayer == player && opponent /= player
            _ -> False
    possibleJumps = [(x + 2, y + 2), (x - 2, y + 2),
                     (x + 2, y - 2), (x - 2, y - 2)]

-- | Checks if a move from one position to another is valid according to the rules
isValidMove :: GameState -> Position -> Position -> Bool
isValidMove (GameState inputBoard inputPlayer _) from@(fromX, fromY) to =
    case inputBoard !! fromY !! fromX of
        Just (Piece player _) ->
            player == inputPlayer && 
            to `elem` getValidMoves (GameState inputBoard inputPlayer (Just from)) from
        Nothing -> False

-- | Updates a single position on the board with a new piece
updateBoard :: Board -> Position -> Maybe Piece -> Board
updateBoard inputBoard (x, y) newPiece =
  take y inputBoard ++
  [take x (inputBoard !! y) ++ [newPiece] ++ drop (x + 1) (inputBoard !! y)] ++
  drop (y + 1) inputBoard

-- | Moves a piece from one position to another on the board.
-- If it's a jump move, removes the captured piece.
movePiece :: Board -> Position -> Position -> Board
movePiece inputBoard (fromX, fromY) (toX, toY) =
  let piece = fromJust (inputBoard !! fromY !! fromX)
      boardWithRemovedPiece = updateBoard inputBoard (fromX, fromY) Nothing
      boardWithMovedPiece = updateBoard boardWithRemovedPiece (toX, toY) (Just piece)
  in if abs (fromX - toX) == 2
     then updateBoard boardWithMovedPiece ((fromX + toX) `div` 2, (fromY + toY) `div` 2) Nothing
     else boardWithMovedPiece

-- | Gets all possible moves for the current player
getAllPossibleMoves :: GameState -> [(Position, [Position])]
getAllPossibleMoves gs@(GameState inputBoard inputPlayer _) = 
    [(pos, getValidMoves gs pos) | pos <- allPiecesPositions]
  where
    allPiecesPositions = [(x, y) | x <- [0..7], y <- [0..7],
                         case inputBoard !! y !! x of
                             Just (Piece player _) -> player == inputPlayer
                             Nothing -> False]

-- | Checks if any piece has a valid jump move
hasValidJumps :: GameState -> Bool
hasValidJumps (GameState inputBoard inputPlayer _) =
    any hasJumps [(x, y) | x <- [0..7], y <- [0..7]]
  where
    hasJumps pos@(x, y) = case inputBoard !! y !! x of
        Just (Piece player _) | player == inputPlayer -> not $ null $ getValidJumpMoves inputBoard pos inputPlayer
        _ -> False

-- | Gets all valid moves for a piece at the given position
getValidMoves :: GameState -> Position -> [Position]
getValidMoves gs@(GameState inputBoard inputPlayer _) pos@(x, y) =
    case inputBoard !! y !! x of
        Just piece@(Piece player _) | player == inputPlayer ->
            if hasValidJumps gs
                then getValidJumpMoves inputBoard pos inputPlayer
                else getValidBasicMoves piece pos
        _ -> []

-- | Gets all valid jump moves for a piece
getValidJumpMoves :: Board -> Position -> Player -> [Position]
getValidJumpMoves inputBoard (x, y) inputPlayer =
    case inputBoard !! y !! x of
        Just piece@(Piece player _) | player == inputPlayer ->
            getJumpPositions inputBoard (x, y) piece
        _ -> []

-- | Gets multiple jump moves for a piece
getMultipleJumpMoves :: Board -> Position -> Player -> [[Position]]
getMultipleJumpMoves inputBoard pos inputPlayer = 
    case inputBoard !! snd pos !! fst pos of
        Just piece ->
            let jumpPositions = getJumpPositions inputBoard pos piece
            in 
                [ pos : (jumpPos : path) | 
                  jumpPos <- jumpPositions,
                  path <- getMultipleJumpMoves (movePiece inputBoard pos jumpPos) jumpPos inputPlayer]
        Nothing -> []

-- | Get all valid basic moves for a piece at a given position
getValidBasicMoves :: Piece -> Position -> [Position]
getValidBasicMoves (Piece player pType) (x, y) =
    let 
        direction = if player == White then 1 else -1
        candidateMoves = 
            case pType of
                King -> 
                    [ (x+dx, y+dy) 
                    | dx <- [-1, 1]
                    , dy <- [direction, -direction]
                    ]
                Man -> 
                    [ (x+dx, y+direction) 
                    | dx <- [-1, 1]
                    ]
        isValidPosition (nx, ny) = nx >= 0 && nx < 8 && ny >= 0 && ny < 8
    in filter isValidPosition candidateMoves
