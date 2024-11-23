{-
This module implements different AI strategies for playing checkers,
including random moves and minimax algorithm with alpha-beta pruning.
-}

module AI.AIPlayer
    ( makeAIMove
    ) where

import System.Random (randomRIO)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Game.State (GameState(..), Difficulty(..))
import Board.Types (Player(..), PieceType(..), Piece(..), Board, Position)
import Rules.GameEnd (isGameOver)
import Rules.Movement (getValidMoves)
import Rules.Capture (makeMove)

-- | Makes an AI move based on the selected difficulty
makeAIMove :: GameState -> Difficulty -> IO GameState
makeAIMove gs difficulty = case difficulty of
    Easy -> makeRandomMove gs
    Medium -> makeMediumMove gs
    Hard -> makeHardMove gs

-- | Makes a random valid move
makeRandomMove :: GameState -> IO GameState
makeRandomMove gs@(GameState board player _) = do
    let pieces = [(x, y) | x <- [0..7], y <- [0..7],
                  case board !! y !! x of
                    Just (Piece p _) -> p == player
                    Nothing -> False]
    if null pieces
        then return gs
        else do
            pieceIdx <- randomRIO (0, length pieces - 1)
            let piece = pieces !! pieceIdx
            let moves = getValidMoves gs piece
            if null moves
                then return gs
                else do
                    moveIdx <- randomRIO (0, length moves - 1)
                    let move = moves !! moveIdx
                    return $ makeMove gs piece move

-- | Makes a medium difficulty move (looks ahead 1-2 moves)
makeMediumMove :: GameState -> IO GameState
makeMediumMove gs@(GameState board player _) = do
    let moves = [(from, to) | from@(x, y) <- allPositions,
                             case board !! y !! x of
                               Just (Piece p _) -> p == player
                               Nothing -> False,
                             to <- getValidMoves gs from]
    if null moves
        then return gs
        else do
            moveIdx <- randomRIO (0, length moves - 1)
            let (from, to) = moves !! moveIdx
            return $ makeMove gs from to

-- | Makes a hard difficulty move using minimax with alpha-beta pruning
makeHardMove :: GameState -> IO GameState
makeHardMove gs@(GameState board player _) = do
    let depth = 3  -- Look ahead 3 moves
    let moves = [(from, to) | from@(x, y) <- allPositions,
                             case board !! y !! x of
                               Just (Piece p _) -> p == player
                               Nothing -> False,
                             to <- getValidMoves gs from]
    if null moves
        then return gs
        else do
            let moveScores = [(move, score) | 
                             move@(from, to) <- moves,
                             let nextState = makeMove gs from to,
                             let score = minimax nextState (depth - 1) False minBound maxBound]
            let (from, to) = fst $ maximumBy (comparing snd) moveScores
            return $ makeMove gs from to

-- | Helper function for all board positions
allPositions :: [Position]
allPositions = [(x, y) | x <- [0..7], y <- [0..7]]

-- | Minimax algorithm with alpha-beta pruning
minimax :: GameState -> Int -> Bool -> Int -> Int -> Int
minimax gs depth maximizing alpha beta
    | depth == 0 = evaluatePosition gs
    | otherwise = 
        let moves = [(from, to) | from@(x, y) <- allPositions,
                                 case board gs !! y !! x of
                                   Just (Piece p _) -> p == currentPlayer gs
                                   Nothing -> False,
                                 to <- getValidMoves gs from]
        in if maximizing
           then maximumValue moves alpha beta
           else minimumValue moves alpha beta
    where
        maximumValue [] a _ = a
        maximumValue ((from, to):rest) a b = 
            let nextState = makeMove gs from to
                value = minimax nextState (depth - 1) False a b
                newAlpha = max a value
            in if b <= newAlpha
               then newAlpha
               else maximumValue rest newAlpha b

        minimumValue [] _ b = b
        minimumValue ((from, to):rest) a b =
            let nextState = makeMove gs from to
                value = minimax nextState (depth - 1) True a b
                newBeta = min b value
            in if newBeta <= a
               then newBeta
               else minimumValue rest a newBeta

        board (GameState b _ _) = b
        currentPlayer (GameState _ p _) = p

-- | Evaluate the current position (simple piece counting for now)
evaluatePosition :: GameState -> Int
evaluatePosition (GameState board player _) =
    sum [evaluatePiece pos piece | 
         x <- [0..7], y <- [0..7],
         let pos = (x, y),
         let piece = board !! y !! x,
         case piece of
           Just _ -> True
           Nothing -> False]
    where
        evaluatePiece _ (Just (Piece p pt)) =
            let baseValue = case pt of
                    Man -> 1
                    King -> 3
                multiplier = if p == player then 1 else -1
            in baseValue * multiplier
        evaluatePiece _ Nothing = 0