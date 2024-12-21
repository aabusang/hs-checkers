-- |
-- Module      : Rules.GameEnd
-- Description : Game end conditions for Checkers
-- 
-- This module handles checking for game end conditions,
-- such as when a player has no valid moves remaining.

module Rules.GameEnd
    ( -- * Game Status
      updateGameStatus
    , isGameOver
      -- * Player Status
    , hasValidMoves
    , countPieces
    ) where

import Board.Types (Player(..), Board, Piece(..))
import Board.Operations (getPieceAt)

import Types.Common (Position)
import Types.Game (GameState(..), GameStatus(..))
import Rules.Movement (getValidBasicMoves)
import Rules.Capture (hasCaptureMoves)


-- _________________________________ GAME STATUS _________________________________

-- | Check if the game is over (either won or drawn)
isGameOver :: GameState -> Bool
isGameOver GameState{gameStatus = status} = 
    case status of
        Ongoing -> False
        _ -> True

-- | Update the game status based on current board state
updateGameStatus :: GameState -> GameState
updateGameStatus gameState =
    let currPlayer = currentPlayer gameState
        newStatus = determineGameStatus gameState currPlayer
    in gameState { gameStatus = newStatus }

-- | Determine the current status of the game
determineGameStatus :: GameState -> Player -> GameStatus
determineGameStatus gameState player
    | isDraw gameState = Draw
    | not (hasValidMoves gameState) = Won (switchPlayer player)
    | otherwise = Ongoing

-- | Check if the game is a draw. A game is drawn when either:
-- 1. No captures have been made in the last 40 moves
-- 2. Only one king remains for each player (no winning possible)
isDraw :: GameState -> Bool
isDraw gameState = 
    hasReachedMoveLimit (movesSinceCapture gameState) || 
    isKingVsKingStalemate (board gameState)

-- | Check if we've reached the 40-move limit without captures
hasReachedMoveLimit :: Int -> Bool
hasReachedMoveLimit moves = moves >= 40

-- | Check if only one king remains for each player, resulting in a stalemate
isKingVsKingStalemate :: Board -> Bool
isKingVsKingStalemate currentBoard = 
    case countPieces currentBoard of
        (1, 1) -> True  -- One piece each
        _ -> False      -- More pieces remain


-- _________________________________ PLAYER STATUS _________________________________

-- | Check if the current player has any valid moves
hasValidMoves :: GameState -> Bool
hasValidMoves (GameState gameBoard player _ _ _ _) =
    any (hasMovesFromPosition gameBoard player) allPlayerPieces
  where
    allPlayerPieces = getPlayerPieces gameBoard player

-- | Check if a player has any moves from a position
hasMovesFromPosition :: Board -> Player -> Position -> Bool
hasMovesFromPosition gameBoard player pos =
    case getPieceAt gameBoard pos of
        Just piece -> isPlayersPiece piece player && hasPieceMoves (GameState gameBoard player undefined undefined undefined undefined) pos
        Nothing -> False


-- | Check if a piece belongs to a player
isPlayersPiece :: Piece -> Player -> Bool
isPlayersPiece (Piece owner _) player = owner == player

-- | Check if a piece has any valid moves
hasPieceMoves :: GameState -> Position -> Bool
hasPieceMoves (GameState gameBoard _ _ _ _ _) pos =
    case getPieceAt gameBoard pos of
        Nothing -> False
        Just piece -> 
            hasCaptureMoves gameBoard pos (pieceOwner piece)  -- Check for captures
            || not (null (getValidBasicMoves piece pos gameBoard))  -- Or any basic moves

-- | Count how many pieces each player has
-- Returns a tuple of (Black pieces, White pieces)
countPieces :: Board -> (Int, Int)
countPieces gameBoard = 
    (length $ getPlayerPieces gameBoard Black, length $ getPlayerPieces gameBoard White)


-- _________________________________ HELPER FUNCTIONS _________________________________

-- | Get all positions with a player's pieces
getPlayerPieces :: Board -> Player -> [Position]
getPlayerPieces gameBoard player = 
    [(row, col) | row <- [0..7], col <- [0..7],
     case gameBoard !! row !! col of
         Just (Piece p _) -> p == player
         Nothing -> False]

-- | Switch to the other player
switchPlayer :: Player -> Player
switchPlayer Black = White
switchPlayer White = Black
