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

import Board.Types (Player(..), Board, Piece(..), PieceType(..))
import Board.Operations (getPieceAt, movePiece, removePiece)
import Types.Common (Position)
import Types.Game (GameState(..))
import Rules.Movement (getValidMoves)
import Rules.Capture (isCaptureMove, getCapturedPosition, getPossibleCaptures)
import Game.State (makeMove)
import AI.Types (Difficulty(..), AIMove, MoveScore)

-- | Select an AI move based on the current game state and difficulty
selectAIMove :: StdGen -> Difficulty -> GameState -> Maybe (AIMove, StdGen)
selectAIMove rng difficulty gameState =
    let validMoves = getAllValidMoves gameState
    in if null validMoves
       then Nothing  -- No valid moves available
       else case difficulty of
            Easy   -> selectEasyMove rng validMoves gameState
            Medium -> selectMediumMove rng validMoves gameState
            Hard   -> selectHardMove rng validMoves gameState

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

-- | Select a move for Easy difficulty (mostly random with slight preference for captures)
selectEasyMove :: StdGen -> [AIMove] -> GameState -> Maybe (AIMove, StdGen)
selectEasyMove rng [] _ = Nothing
selectEasyMove rng moves gameState =
    let 
        -- Separate capture and non-capture moves
        captureMoves = filter (isCapture gameState) moves
        normalMoves = filter (not . isCapture gameState) moves
        
        -- 70% chance to choose a capture move if available, otherwise random
        (randomVal, rng1) = randomR (1 :: Int, 10) rng
        preferCaptures = randomVal <= 7  -- 70% chance
        
        -- Choose which move list to use
        movesToUse = if not (null captureMoves) && preferCaptures
                     then captureMoves
                     else moves
                     
        -- Pick a random move from the selected list
        (index, rng2) = randomR (0, length movesToUse - 1) rng1
        selectedMove = movesToUse !! index
    in Just (selectedMove, rng2)

-- | Select a move for Medium difficulty (evaluates one move ahead)
selectMediumMove :: StdGen -> [AIMove] -> GameState -> Maybe (AIMove, StdGen)
selectMediumMove rng [] _ = Nothing
selectMediumMove rng moves gameState = selectBestMoveOneStep rng moves gameState

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

-- | Select a move for Hard difficulty (looks two moves ahead and uses better evaluation)
selectHardMove :: StdGen -> [AIMove] -> GameState -> Maybe (AIMove, StdGen)
selectHardMove rng [] _ = Nothing
selectHardMove rng moves gameState =
    let 
        -- Score each move with a deeper evaluation
        scoredMoves = map (\move -> (move, scoreHardMove move gameState)) moves
        
        -- Sort moves by score (highest first)
        sortedMoves = sortOn (Down . snd) scoredMoves
        
        -- Get top moves (those with the same highest score)
        bestScore = snd (head sortedMoves)
        topMoves = takeWhile ((\s -> abs (bestScore - s) <= 2) . snd) sortedMoves  -- Allow some variation
        
        -- Select randomly from top moves
        (index, newRng) = randomR (0, length topMoves - 1) rng
        (selectedMove, _) = topMoves !! index
    in Just (selectedMove, newRng)

-- | Score a move based on its immediate outcome (for Medium difficulty)
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

-- | Score a move for Hard difficulty (more sophisticated evaluation)
scoreHardMove :: AIMove -> GameState -> MoveScore
scoreHardMove move@(fromPos, toPos) gameState =
    let 
        -- Get the basic score first
        basicScore = scoreMove move gameState
        
        -- Simulate making this move
        simulatedState = simulateMove move gameState
        
        -- Check if this move leads to a king
        kingScore = case simulatedState of
            Just state -> 
                case getPieceAt (board state) toPos of
                    Just piece -> if pieceType piece == King then 5 else 0
                    Nothing -> 0
            Nothing -> 0
            
        -- Look ahead to see opponent's best response
        opponentScore = case simulatedState of
            Just state -> 
                let opponentMoves = getAllValidMoves state
                in if null opponentMoves
                   then 20  -- Win if opponent has no moves
                   else negate $ maximum $ map (\m -> scoreMove m state) opponentMoves
            Nothing -> 0
            
        -- Evaluate board control (center control and edge avoidance)
        controlScore = evaluateBoardControl toPos
        
    in basicScore + kingScore + (opponentScore `div` 2) + controlScore

-- | Evaluate how good a position is for board control
evaluateBoardControl :: Position -> MoveScore
evaluateBoardControl (row, col) =
    let 
        -- Center control is good
        centerScore = if row >= 2 && row <= 5 && col >= 2 && col <= 5 then 2 else 0
        
        -- Edge positions are vulnerable (except for kings)
        edgeScore = if row == 0 || row == 7 || col == 0 || col == 7 then -1 else 0
    in centerScore + edgeScore

-- | Simulate making a move and return the resulting game state
simulateMove :: AIMove -> GameState -> Maybe GameState
simulateMove (fromPos, toPos) gameState = makeMove gameState fromPos toPos

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
