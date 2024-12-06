-- |
-- Module      : Game.State
-- Description : Game state management
-- 
-- This module contains the basic game operations like:
-- * Starting a new game
-- * Selecting pieces
-- * Making moves

module Game.State
    ( -- * Game State
      GameState(..)
    , initialGameState
      -- * Game Actions
    , selectPiece
    , makeMove
    , startNewGame
    ) where

import Board.Construction (initialBoard)
import Board.Types (Player(..), Piece(..))
import Board.Operations (getPieceAt, movePiece)
import Rules.Movement (getValidMoves, isValidMove)
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
  gameState {
    board            = movePiece (board gameState) fromPos toPos,
    currentPlayer    = switchPlayer (currentPlayer gameState),
    selectedPiecePos = Nothing,  -- Deselect the piece
    validMoves       = []        -- Clear valid moves
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
    if Rules.Movement.isValidMove gameState fromPos toPos
        then Just (createNewGameState gameState fromPos toPos)
        else Nothing


-- | Select piece if position is valid and is owned by current player
selectPiece :: GameState -> Position -> Maybe GameState
selectPiece gameState pos =
    let maybePiece = getPieceAt (board gameState) pos
    in case maybePiece of
        Just piece | pieceIsOwnedByCurrentPlayer gameState piece ->
            Just $  gameState {
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
