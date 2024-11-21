module Game where

import Data.Maybe (isJust, fromJust)

data Player = Black | White deriving (Show, Eq)
data PieceType = Man | King deriving (Show, Eq)
data Piece = Piece Player PieceType deriving (Show, Eq)
type Position = (Int, Int)
type Board = [[Maybe Piece]]

data GameState = GameState
  { board :: Board
  , currentPlayer :: Player
  , selectedPiece :: Maybe Position
  }

initialBoard :: Board
initialBoard = [
    [Just (Piece Black Man), Nothing, Just (Piece Black Man), Nothing, Just (Piece Black Man), Nothing, Just (Piece Black Man), Nothing],
    [Nothing, Just (Piece Black Man), Nothing, Just (Piece Black Man), Nothing, Just (Piece Black Man), Nothing, Just (Piece Black Man)],
    [Just (Piece Black Man), Nothing, Just (Piece Black Man), Nothing, Just (Piece Black Man), Nothing, Just (Piece Black Man), Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Just (Piece White Man), Nothing, Just (Piece White Man), Nothing, Just (Piece White Man), Nothing, Just (Piece White Man)],
    [Just (Piece White Man), Nothing, Just (Piece White Man), Nothing, Just (Piece White Man), Nothing, Just (Piece White Man), Nothing],
    [Nothing, Just (Piece White Man), Nothing, Just (Piece White Man), Nothing, Just (Piece White Man), Nothing, Just (Piece White Man)]
  ]

initialGameState :: GameState
initialGameState = GameState initialBoard Black Nothing

oppositePlayer :: Player -> Player
oppositePlayer Black = White
oppositePlayer White = Black

isValidMove :: GameState -> Position -> Position -> Bool
isValidMove (GameState board player _) from to =
  let (fromX, fromY) = from
      (toX, toY) = to
      piece = board !! fromY !! fromX
  in case piece of
       Just (Piece p pieceType) | p == player ->
         let dx = toX - fromX
             dy = toY - fromY
             direction = if player == Black then 1 else -1
             validDirections = case pieceType of
               Man -> [direction]
               King -> [1, -1]
         in (abs dx == 1 && dy `elem` validDirections) || -- normal move
            (abs dx == 2 && abs dy == 2 && -- jump
             isJust (board !! (fromY + dy `div` 2) !! (fromX + dx `div` 2)) &&
             case board !! (fromY + dy `div` 2) !! (fromX + dx `div` 2) of
               Just (Piece p' _) -> p' /= player
               _ -> False)
       _ -> False

makeMove :: GameState -> Position -> Position -> GameState
makeMove gs@(GameState board player _) from to =
  if isValidMove gs from to
  then let newBoard = movePiece board from to
           promotedBoard = promoteKings newBoard
           hasMoreJumps = canJumpAgain promotedBoard to player
           newPlayer = if hasMoreJumps then player else oppositePlayer player
           newSelected = if hasMoreJumps then Just to else Nothing
       in GameState promotedBoard newPlayer newSelected
  else gs

promoteKings :: Board -> Board
promoteKings board =
  [[case piece of
      Just (Piece Black Man) | y == 7 -> Just (Piece Black King)
      Just (Piece White Man) | y == 0 -> Just (Piece White King)
      p -> p
    | (x, piece) <- zip [0..] row]
   | (y, row) <- zip [0..] board]

canJumpAgain :: Board -> Position -> Player -> Bool
canJumpAgain board pos player =
  let moves = getValidMoves (GameState board player (Just pos)) pos
  in any (\(toX, toY) -> abs (fst pos - toX) == 2) moves

movePiece :: Board -> Position -> Position -> Board
movePiece board (fromX, fromY) (toX, toY) =
  let piece = fromJust (board !! fromY !! fromX)
      newBoard = updateBoard board (fromX, fromY) Nothing
      finalBoard = updateBoard newBoard (toX, toY) (Just piece)
  in if abs (toX - fromX) == 2 -- it's a jump
     then updateBoard finalBoard ((fromX + toX) `div` 2, (fromY + toY) `div` 2) Nothing
     else finalBoard

updateBoard :: Board -> Position -> Maybe Piece -> Board
updateBoard board (x, y) piece =
  take y board ++
  [take x (board !! y) ++ [piece] ++ drop (x + 1) (board !! y)] ++
  drop (y + 1) board

getValidMoves :: GameState -> Position -> [Position]
getValidMoves gs@(GameState board player _) (x, y) =
  case board !! y !! x of
    Just (Piece p _) | p == player ->
      let directions = if p == Black then [(1, 1), (-1, 1)] else [(1, -1), (-1, -1)]
          normalMoves = [(x + dx, y + dy) | (dx, dy) <- directions, isValidMove gs (x, y) (x + dx, y + dy)]
          jumpMoves = [(x + 2*dx, y + 2*dy) | (dx, dy) <- directions, isValidMove gs (x, y) (x + 2*dx, y + 2*dy)]
      in normalMoves ++ jumpMoves
    _ -> []

getAllValidMoves :: GameState -> [(Position, [Position])]
getAllValidMoves gs@(GameState board player _) =
  [(pos, moves) | x <- [0..7], y <- [0..7], 
   let pos = (x, y), 
   let moves = getValidMoves gs pos,
   not (null moves)]

isGameOver :: GameState -> Bool
isGameOver gs = null (getAllValidMoves gs)