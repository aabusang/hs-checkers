{-
This module implements different AI strategies for playing checkers,
including random moves and minimax algorithm with alpha-beta pruning.
-}

module AI.AIPlayer
    ( makeAIMove
    ) where

import Data.List (maximumBy)
import Data.Ord (comparing)
import System.Random (randomRIO)

import Game.State (GameState(..))
import Game.Mode (Difficulty(..))
import Board.Types (Player(..), Position, Piece(..))
import Rules.Movement (getValidMoves)
import Rules.Capture (makeMove)

-- | Makes an AI move based on the selected difficulty
makeAIMove :: GameState -> Difficulty -> IO (Maybe GameState)
makeAIMove gs difficulty = case difficulty of
    Easy -> makeRandomMove gs
    Medium -> makeMediumMove gs
    Hard -> makeHardMove gs

-- | Makes a random valid move
makeRandomMove :: GameState -> IO (Maybe GameState)
makeRandomMove gs@(GameState _ _ _) = do
    let ms = getAllPossibleMoves gs
    if null ms
        then return Nothing
        else do
            idx <- randomRIO (0, length ms - 1)
            let (f, ts) = ms !! idx
            if null ts
                then return Nothing
                else do
                    tIdx <- randomRIO (0, length ts - 1)
                    let t = ts !! tIdx
                    return $ Just $ makeMove gs f t

-- | Makes a medium difficulty move
makeMediumMove :: GameState -> IO (Maybe GameState)
makeMediumMove gs@(GameState _ _ _) = do
    let ms = getAllPossibleMoves gs
    if null ms
        then return Nothing
        else do
            let scoredMs = [(f, t, evaluateMove gs f t) | (f, ts) <- ms, t <- ts]
            if null scoredMs
                then return Nothing
                else do
                    let bestScore = maximum $ map (\(_, _, score) -> score) scoredMs
                    let bestMs = [(f, t) | (f, t, score) <- scoredMs, score == bestScore]
                    idx <- randomRIO (0, length bestMs - 1)
                    let (f, t) = bestMs !! idx
                    return $ Just $ makeMove gs f t

-- | Makes a hard difficulty move
makeHardMove :: GameState -> IO (Maybe GameState)
makeHardMove gs@(GameState _ _ _) = do
    let ms = getAllPossibleMoves gs
    if null ms
        then return Nothing
        else do
            let scoredMs = [(f, t, evaluateMove gs f t) | (f, ts) <- ms, t <- ts]
            if null scoredMs
                then return Nothing
                else do
                    let (f, t, _) = maximumBy (comparing (\(_, _, score) -> score)) scoredMs
                    return $ Just $ makeMove gs f t

-- | Evaluate a move's strength
evaluateMove :: GameState -> Position -> Position -> Int
evaluateMove gs f t =
    let newGs = makeMove gs f t
    in evaluatePosition newGs

-- | Evaluate a position's strength for the current player
evaluatePosition :: GameState -> Int
evaluatePosition (GameState b p _) =
    sum [pieceValue p (b !! y !! x) | x <- [0..7], y <- [0..7]]

-- | Helper function for all board positions
allPositions :: [Position]
allPositions = [(x, y) | x <- [0..7], y <- [0..7]]

-- | Helper function to get all possible moves
getAllPossibleMoves :: GameState -> [(Position, [Position])]
getAllPossibleMoves gs@(GameState b p _) = 
    [(f, ts) | f@(x, y) <- allPositions,
               isPieceOfPlayer p (b !! y !! x),
               let ts = getValidMoves gs f]

-- | Evaluate piece value for a given player
pieceValue :: Player -> Maybe Piece -> Int
pieceValue p (Just (Piece player _)) = if player == p then 1 else -1
pieceValue _ Nothing = 0

-- | Determine if a piece belongs to the current player
isPieceOfPlayer :: Player -> Maybe Piece -> Bool
isPieceOfPlayer p (Just (Piece player _)) = player == p
isPieceOfPlayer _ Nothing = False