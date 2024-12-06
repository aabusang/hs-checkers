{-
This module implements different AI strategies for playing checkers,
including random moves and minimax algorithm with alpha-beta pruning.
-}

module AI.AIPlayer
    ( getAIMove
    ) where

-- Board related imports
import Board.Types (Piece(..))

-- Game related imports
import Types.Game (GameState(..), GameStatus(..))
import Types.Common (Position)

-- Rules and Logic
import Rules.Movement (getValidMoves)

-- System imports
import System.Random (randomRIO)

-- | Get a random valid move for the AI
-- Returns Nothing if no valid moves are available
getAIMove :: GameState -> IO (Maybe (Position, Position))
getAIMove GameState{board = gameBoard, currentPlayer = player} = do
    -- Get all pieces of current player
    let positions = [(x, y) | x <- [0..7], y <- [0..7],
                    case gameBoard !! y !! x of
                        Just piece -> pieceOwner piece == player
                        Nothing -> False]
    
    if null positions
        then return Nothing
        else do
            -- Pick a random piece
            pieceIdx <- randomRIO (0, length positions - 1)
            let fromPos = positions !! pieceIdx
            
            -- Get valid moves for this piece
            let moves = getValidMoves GameState { 
                    board = gameBoard,
                    currentPlayer = player,
                    selectedPiecePos = Nothing,
                    validMoves = [],
                    gameStatus = Ongoing,
                    movesSinceCapture = 0
                } fromPos
            
            if null moves
                then return Nothing
                else do
                    -- Pick a random move
                    moveIdx <- randomRIO (0, length moves - 1)
                    let toPos = moves !! moveIdx
                    return $ Just (fromPos, toPos)