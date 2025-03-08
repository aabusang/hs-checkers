-- |
-- Module      : AI.Engine
-- Description : AI move selection engine
-- 
-- This module contains the core AI functionality for selecting moves
-- based on different difficulty levels.

module AI.Engine
    ( -- * AI Move Selection
      selectAIMove
    ) where

import System.Random (StdGen, randomR)
import Data.List (sortOn)
import Data.Ord (Down(..))

import Board.Types (Player(..), Board, Piece(..))
import Board.Operations (getPieceAt, movePiece, removePiece)
import Types.Common (Position)
import Types.Game (GameState(..))
import Rules.Movement (getValidMoves)
import Rules.Capture (isCaptureMove, getCapturedPosition, getPossibleCaptures)
import AI.Types (Difficulty(..), AIMove, MoveScore)

-- | Select an AI move based on the current game state and difficulty
selectAIMove :: StdGen -> Difficulty -> GameState -> Maybe (AIMove, StdGen)
selectAIMove rng difficulty gameState =
    let validMoves = getAllValidMoves gameState
    in if null validMoves
       then Nothing  -- No valid moves available
       else case difficulty of
            Easy   -> selectRandomMove rng validMoves
            Medium -> selectBestMoveOneStep rng validMoves gameState
            Hard   -> selectBestMoveOneStep rng validMoves gameState  -- For simplicity, same as medium for now

-- | Get all valid moves for the current player
getAllValidMoves :: GameState -> [AIMove]
getAllValidMoves gameState =
    let player = currentPlayer gameState
        currentBoard = board gameState
        playerPieces = getPlayerPieces currentBoard player
        
        -- Get all possible moves for each piece
        allMoves = concatMap (getMovesForPiece gameState) playerPieces
        
        -- If capture moves are available, only return those
        captureMoves = filter (isCapture gameState) allMoves
    in if not (null captureMoves)
       then captureMoves  -- Must make capture moves when available
       else allMoves

-- | Get all valid moves for a specific piece
getMovesForPiece :: GameState -> Position -> [AIMove]
getMovesForPiece gameState fromPos =
    let validDestinations = getValidMoves gameState fromPos
    in map (\toPos -> (fromPos, toPos)) validDestinations

-- | Check if a move is a capture move
isCapture :: GameState -> AIMove -> Bool
isCapture gameState (fromPos, toPos) = isCaptureMove fromPos toPos

-- | Select a random move from the list of valid moves
selectRandomMove :: StdGen -> [AIMove] -> Maybe (AIMove, StdGen)
selectRandomMove rng [] = Nothing
selectRandomMove rng moves =
    let (index, newRng) = randomR (0, length moves - 1) rng
        selectedMove = moves !! index
    in Just (selectedMove, newRng)

-- | Select the best move by looking one step ahead
selectBestMoveOneStep :: StdGen -> [AIMove] -> GameState -> Maybe (AIMove, StdGen)
selectBestMoveOneStep rng [] _ = Nothing
selectBestMoveOneStep rng moves gameState =
    let 
        -- Score each move
        scoredMoves = map (\move -> (move, scoreMove move gameState)) moves
        
        -- Sort moves by score (highest first)
        sortedMoves = sortOn (Down . snd) scoredMoves
        
        -- Get top moves (those with the same highest score)
        bestScore = snd (head sortedMoves)
        topMoves = takeWhile ((== bestScore) . snd) sortedMoves
        
        -- Select randomly from top moves
        (index, newRng) = randomR (0, length topMoves - 1) rng
        (selectedMove, _) = topMoves !! index
    in Just (selectedMove, newRng)

-- | Score a move based on its immediate outcome
scoreMove :: AIMove -> GameState -> MoveScore
scoreMove (fromPos, toPos) gameState =
    let 
        currentBoard = board gameState
        player = currentPlayer gameState
        
        -- Base score
        baseScore = 0
        
        -- Add points for captures
        captureScore = if isCaptureMove fromPos toPos
                      then 10  -- Capturing is generally good
                      else 0
        
        -- Add points for advancing pawns (for both players)
        advanceScore = case player of
            Black -> if snd fromPos /= snd toPos  -- If it's a diagonal move
                    then fromPos `advanceValue` toPos
                    else 0
            White -> if snd fromPos /= snd toPos  -- If it's a diagonal move
                    then toPos `advanceValue` fromPos
                    else 0
        
        -- Add points for protecting pieces
        protectionScore = 0  -- Simplified for now
        
    in baseScore + captureScore + advanceScore + protectionScore

-- | Calculate the value of advancing a piece
advanceValue :: Position -> Position -> MoveScore
advanceValue (r1, _) (r2, _) = abs (r1 - r2)

-- | Get all positions with a player's pieces
getPlayerPieces :: Board -> Player -> [Position]
getPlayerPieces gameBoard player = 
    [(row, col) | row <- [0..7], col <- [0..7],
     case getPieceAt gameBoard (row, col) of
         Just piece -> pieceOwner piece == player
         Nothing -> False]
