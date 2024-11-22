{-|
Module      : AI
Description : AI player implementation for the Checkers game
Copyright   : (c) Your Name, 2024
License     : Your License

This module implements different AI strategies for playing checkers,
including random moves and minimax algorithm with alpha-beta pruning.
-}
module AI
    ( makeAIMove
    , Difficulty(..)
    ) where

import System.Random (randomRIO)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Types
import Rules (isValidMove, isGameOver, getValidMoves, makeMove)

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
                   _ -> False]
    if null pieces
    then return gs
    else do
        pieceIndex <- randomRIO (0, length pieces - 1)
        let piece = pieces !! pieceIndex
        let moves = getValidMoves gs piece
        if null moves
        then makeRandomMove gs  -- Try again with a different piece
        else do
            moveIndex <- randomRIO (0, length moves - 1)
            let move = moves !! moveIndex
            return $ makeMove gs piece move

-- | Makes a medium difficulty move using a simple heuristic
makeMediumMove :: GameState -> IO GameState
makeMediumMove gs@(GameState board player _) = do
    let pieces = [(x, y) | x <- [0..7], y <- [0..7],
                 case board !! y !! x of
                   Just (Piece p _) -> p == player
                   _ -> False]
    if null pieces
    then return gs
    else do
        let moves = concatMap (\p -> map (\to -> (p, to)) (getValidMoves gs p)) pieces
        if null moves
        then return gs
        else do
            let scoredMoves = map (\m -> (m, scoreMove gs m)) moves
            let bestMove = maximumBy (comparing snd) scoredMoves
            let (from, to) = fst bestMove
            return $ makeMove gs from to

-- | Makes a hard difficulty move using minimax with alpha-beta pruning
makeHardMove :: GameState -> IO GameState
makeHardMove gs = do
    let depth = 4  -- Adjust depth for different difficulty levels
    let moves = getAllPossibleMoves gs
    if null moves
    then return gs
    else do
        let scoredMoves = map (\(from, to) -> 
                ((from, to), minimax (makeMove gs from to) depth True minBound maxBound))
                moves
        let bestMove = maximumBy (comparing snd) scoredMoves
        let (from, to) = fst bestMove
        return $ makeMove gs from to

-- | Gets all possible moves for the current player
getAllPossibleMoves :: GameState -> [(Position, Position)]
getAllPossibleMoves gs@(GameState board player _) =
    [(from, to) | 
     x <- [0..7], y <- [0..7],
     let from = (x, y),
     case board !! y !! x of
         Just (Piece p _) | p == player -> True
         _ -> False,
     to <- getValidMoves gs from]

-- | Scores a move based on simple heuristics
scoreMove :: GameState -> (Position, Position) -> Int
scoreMove gs (from, to) =
    let newGs@(GameState _ currentPlayer _) = makeMove gs from to
        (fromX, fromY) = from
        (toX, toY) = to
        isJump = abs (fromX - toX) == 2
        pieceValue = case board newGs !! toY !! toX of
            Just (Piece _ King) -> 3
            Just (Piece _ Man) -> 1
            Nothing -> 0
        positionValue = if currentPlayer == Black
                       then toY  -- Black wants to move down
                       else 7 - toY  -- White wants to move up
    in pieceValue * 10 + positionValue + if isJump then 20 else 0

-- | Minimax algorithm with alpha-beta pruning
minimax :: GameState -> Int -> Bool -> Int -> Int -> Int
minimax gs depth isMax alpha beta
  | isGameOver gs = if isMax then (minBound :: Int) else (maxBound :: Int)
  | depth == 0 = evaluatePosition gs
  | otherwise =
      let moves = getAllPossibleMoves gs
          initialValue = if isMax then (minBound :: Int) else (maxBound :: Int)
          nextStates = map (\(from, to) -> makeMove gs from to) moves
      in if isMax
         then maximum $ map (\state -> minimax state (depth - 1) False alpha beta) nextStates
         else minimum $ map (\state -> minimax state (depth - 1) True alpha beta) nextStates

-- | Evaluates a position using material and positional heuristics
evaluatePosition :: GameState -> Int
evaluatePosition (GameState board player _) =
    sum [evaluatePiece (x, y) piece | 
         x <- [0..7], y <- [0..7],
         let piece = board !! y !! x,
         case piece of
             Just _ -> True
             Nothing -> False]
  where
    evaluatePiece (x, y) (Just (Piece p t)) =
        let baseValue = case t of
                King -> 30
                Man -> 10
            positionValue = case p of
                Black -> y  -- Black wants to advance down
                White -> 7 - y  -- White wants to advance up
            multiplier = if p == player then 1 else -1
        in (baseValue + positionValue) * multiplier
    evaluatePiece _ Nothing = 0