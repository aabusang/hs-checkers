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
    , makeMove
    , selectPiece
      -- * Game State Access
    , board
    , currentPlayer
    , selectedPiecePos
    , validMoves
      -- * Game State Helpers
    , pieceIsOwnedByCurrentPlayer
    ) where

import Board.Types (Player(..), Piece(..), Board)
import Board.Operations (getPieceAt, movePiece, removePiece)
import Board.Construction (initialBoard)
import Rules.Movement (getValidMoves, isValidMove)
import Rules.Capture (isCaptureMove, getCapturedPosition, getPossibleCaptures)
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

        -- Check if there are more captures available from the new position
        moreCaptures = if isCaptureMove fromPos toPos
                      then not $ null $ getPossibleCaptures finalBoard toPos (currentPlayer gameState)
                      else False

        -- Only switch player if no more captures are available
        nextPlayer = if moreCaptures
                    then currentPlayer gameState  -- Keep same player for multi-capture
                    else switchPlayer (currentPlayer gameState)

        -- If there are more captures, only show moves from the current piece
        nextValidMoves = if moreCaptures
                        then getPossibleCaptures finalBoard toPos (currentPlayer gameState)
                        else []

        -- Keep the piece selected if there are more captures
        nextSelectedPos = if moreCaptures
                         then Just toPos
                         else Nothing

    in gameState {
        board = finalBoard,
        currentPlayer = nextPlayer,
        selectedPiecePos = nextSelectedPos,
        validMoves = nextValidMoves
    }


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
    let capturesAvailable = any (hasCaptureMove (board gameState)) [(row, col) | row <- [0..7], col <- [0..7],
                       case getPieceAt (board gameState) (row, col) of
                           Just piece -> pieceOwner piece == currentPlayer gameState
                           Nothing -> False]
        isCapture = isCaptureMove fromPos toPos
        -- For multi-capture, only allow moves from the selected piece
        isValidMultiCapture = case selectedPiecePos gameState of
            Just selected -> fromPos == selected  -- Must continue with same piece
            Nothing -> True
    in if capturesAvailable && not isCapture
       then Nothing  -- Must make a capture move when available
       else if not isValidMultiCapture
            then Nothing  -- Must continue with same piece for multi-capture
            else if isValidMove gameState fromPos toPos
                 then Just (createNewGameState gameState fromPos toPos)
                 else Nothing

-- | Select piece if position is valid and is owned by current player
selectPiece :: GameState -> Position -> Maybe GameState
selectPiece gameState pos =
    let currentBoard = board gameState
        maybePiece = getPieceAt currentBoard pos
        capturesAvailable = any (hasCaptureMove currentBoard) [(row, col) | row <- [0..7], col <- [0..7],
                       case getPieceAt currentBoard (row, col) of
                           Just piece -> pieceOwner piece == currentPlayer gameState
                           Nothing -> False]
        -- For multi-capture, only allow selecting the piece that just captured
        isValidSelection = case selectedPiecePos gameState of
            Just selected -> pos == selected || not capturesAvailable
            Nothing -> True
    in case maybePiece of
        Just piece | pieceIsOwnedByCurrentPlayer gameState piece && isValidSelection ->
            if capturesAvailable && not (hasCaptureMove currentBoard pos)
            then Nothing  -- Must select a piece that can capture
            else Just $ gameState {
                selectedPiecePos = Just pos,
                validMoves = getValidMoves gameState pos
            }
        _ -> Nothing



-- | Check if a piece at the given position has any capture moves
hasCaptureMove :: Board -> Position -> Bool
hasCaptureMove currentBoard pos =
    case getPieceAt currentBoard pos of
        Just piece -> not $ null $ getPossibleCaptures currentBoard pos (pieceOwner piece)
        Nothing -> False

-- | Check if a piece belongs to the current player
pieceIsOwnedByCurrentPlayer :: GameState -> Piece -> Bool
pieceIsOwnedByCurrentPlayer gameState piece = pieceOwner piece == currentPlayer gameState


-- | Helper function to switch between players
switchPlayer :: Player -> Player
switchPlayer Black = White
switchPlayer White = Black
