module AI (Difficulty(..), aiMove, makeAIMove) where

import Game
import System.Random
import System.IO.Unsafe (unsafePerformIO)

data Difficulty = Easy | Medium | Hard deriving (Show, Eq)

aiMove :: Difficulty -> GameState -> IO GameState
aiMove difficulty gs = do
  let moves = getAllValidMoves gs
  case difficulty of
    Easy -> randomMove moves gs
    Medium -> do
      r <- randomRIO (1 :: Int, 10 :: Int)
      if r <= 7
        then bestMove 2 gs
        else randomMove moves gs
    Hard -> bestMove 4 gs

randomMove :: [(Position, [Position])] -> GameState -> IO GameState
randomMove moves gs = do
  if null moves
    then return gs
    else do
      i <- randomRIO (0, length moves - 1)
      let (from, tos) = moves !! i
      j <- randomRIO (0, length tos - 1)
      let to = tos !! j
      return $ makeMove gs from to

bestMove :: Int -> GameState -> IO GameState
bestMove depth gs = do
  let moves = getAllValidMoves gs
  if null moves
    then return gs
    else do
      let scoredMoves = map (\(from, tos) -> 
                              maximum $ map (\to -> 
                                (from, to, minimax depth (makeMove gs from to) (oppositePlayer (currentPlayer gs)) False)) tos) moves
      let (bestFrom, bestTo, _) = maximum scoredMoves
      return $ makeMove gs bestFrom bestTo

makeAIMove :: Difficulty -> GameState -> GameState
makeAIMove diff gs = case getBestMove diff gs of
    Just move -> makeMove gs (fst move) (snd move)
    Nothing -> gs  -- No valid moves available

getBestMove :: Difficulty -> GameState -> Maybe (Position, Position)
getBestMove Easy gs = case getRandomMove gs of
    Just move -> Just move
    Nothing -> Nothing
getBestMove Medium gs = case getRandomMove gs of
    Just move -> Just move
    Nothing -> Nothing
getBestMove Hard gs = case getRandomMove gs of
    Just move -> Just move
    Nothing -> Nothing

getRandomMove :: GameState -> Maybe (Position, Position)
getRandomMove gs =
    let moves = getAllPossibleMoves gs
    in if null moves
       then Nothing
       else Just $ moves !! (unsafePerformIO $ randomRIO (0, length moves - 1))

-- Get all possible moves for the current player
getAllPossibleMoves :: GameState -> [(Position, Position)]
getAllPossibleMoves gs@(GameState board player _) =
  [(from, to) | 
    x <- [0..7], y <- [0..7],
    let from = (x, y),
    case board !! y !! x of
      Just (Piece p _) | p == player -> True
      _ -> False,
    to <- [(x+dx, y+dy) | dx <- [-2,-1,1,2], dy <- [-2,-1,1,2]],
    isValidMove gs from to]

minimax :: Int -> GameState -> Player -> Bool -> Int
minimax 0 gs player isMax = evaluateBoard gs player
minimax depth gs player isMax
  | isGameOver gs = if isMax then minBound else maxBound
  | otherwise =
      let moves = getAllValidMoves gs
          nextStates = [makeMove gs from to | (from, tos) <- moves, to <- tos]
          nextScores = map (\nextGs -> minimax (depth - 1) nextGs player (not isMax)) nextStates
      in if isMax then maximum nextScores else minimum nextScores

evaluateBoard :: GameState -> Player -> Int
evaluateBoard (GameState board player _) forPlayer =
  sum [pieceValue p | row <- board, Just p <- row]
  where
    pieceValue (Piece p Man) = if p == forPlayer then 1 else -1
    pieceValue (Piece p King) = if p == forPlayer then 2 else -2