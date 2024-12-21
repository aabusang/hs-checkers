-- |
-- Module      : Game.State
-- Description : Game state management
-- 
-- This module contains the basic game operations like:
-- * Starting a new game
-- * Selecting pieces
-- * Making moves

module Game.State
    ( -- * Game State Management
      initialGameState
    , createNewGameState
    , makeMove
    , selectPiece
      -- * Game State Helpers
    , pieceIsOwnedByCurrentPlayer
    , switchPlayer
    ) where

import Board.Types (Player(..), Piece(..), Board)
import Board.Operations (getPieceAt, movePiece, removePiece)
import Board.Construction (initialBoard)
import Rules.Movement (getValidMoves, isValidMove, isCaptureMove, getCapturedPosition, getValidBasicMoves, getPossibleCaptures)
import Types.Common (Position)
import Types.Game (GameState(..), GameStatus(..))

-- | Creates a new game state.
-- The game always starts with:
-- * The initial board setup
-- * Black player's turn
-- * No piece selected
-- * No valid moves
-- * Game status is Ongoing
-- * No moves made since last capture
initialGameState :: GameState
initialGameState = GameState initialBoard Black Nothing [] Ongoing 0

-- | Start a new game
-- This is the same as initialGameState, just with a more descriptive name
startNewGame :: GameState
startNewGame = initialGameState



-- | Create a new game state after a successful move
createNewGameState :: GameState -> Position -> Position -> GameState
createNewGameState gameState fromPos toPos = 
    -- First move the piece
    let currentBoard = board gameState
        newBoard = movePiece currentBoard fromPos toPos
        
        -- If it's a capture move, remove the captured piece
        finalBoard = if isCaptureMove fromPos toPos
                    then 
                        let capturedPos = getCapturedPosition fromPos toPos
                        in removePiece capturedPos newBoard
                    else newBoard
    in gameState {
        board = finalBoard,
        currentPlayer = switchPlayer (currentPlayer gameState),
        selectedPiecePos = Nothing,
        validMoves = []
    }


-- | Check if any piece of the current player has a capture move available
hasCaptureMoveAvailable :: GameState -> Bool
hasCaptureMoveAvailable gameState =
    let currentBoard = board gameState
        playerPieces = [(row, col) | row <- [0..7], col <- [0..7],
                       case getPieceAt currentBoard (row, col) of
                           Just piece -> pieceOwner piece == currentPlayer gameState
                           Nothing -> False]
    in any (hasCaptureMove currentBoard) playerPieces

-- | Check if a piece at the given position has any capture moves
hasCaptureMove :: Board -> Position -> Bool
hasCaptureMove currentBoard pos =
    case getPieceAt currentBoard pos of
        Just piece -> not $ null $ getPossibleCaptures currentBoard pos (pieceOwner piece)
        Nothing -> False

-- | Try to make a move from one position to another.
-- Returns Nothing if:
-- * The move is invalid
--
-- Returns Just newState if the move is valid, with:
-- * Updated board with the piece moved
-- * Next player's turn
-- * No piece selected
-- * Empty valid moves list
makeMove :: GameState -> Position -> Position -> Maybe GameState
makeMove gameState fromPos toPos = 
    let capturesAvailable = hasCaptureMoveAvailable gameState
        isCapture = isCaptureMove fromPos toPos
    in if capturesAvailable && not isCapture
       then Nothing  -- Must make a capture move when available
       else if Rules.Movement.isValidMove gameState fromPos toPos
            then Just (createNewGameState gameState fromPos toPos)
            else Nothing

-- | Select piece if position is valid and is owned by current player
selectPiece :: GameState -> Position -> Maybe GameState
selectPiece gameState pos =
    let currentBoard = board gameState
        maybePiece = getPieceAt currentBoard pos
        capturesAvailable = hasCaptureMoveAvailable gameState
    in case maybePiece of
        Just piece | pieceIsOwnedByCurrentPlayer gameState piece ->
            if capturesAvailable && not (hasCaptureMove currentBoard pos)
            then Nothing  -- Must select a piece that can capture
            else Just $ gameState {
                selectedPiecePos = Just pos,
                validMoves = getValidMoves gameState pos
            }
        _ -> Nothing



-- | Check if a piece belongs to the current player
pieceIsOwnedByCurrentPlayer :: GameState -> Piece -> Bool
pieceIsOwnedByCurrentPlayer gameState piece = pieceOwner piece == currentPlayer gameState


-- | Helper function to switch between players
switchPlayer :: Player -> Player
switchPlayer Black = White
switchPlayer White = Black
