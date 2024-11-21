module UI where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Game
import AI

data GameMode = SinglePlayer Difficulty | TwoPlayer deriving (Show, Eq)

data UIState = UIState
  { gameState :: GameState
  , gameMode :: GameMode
  }

windowSize :: Int
windowSize = 800

cellSize :: Float
cellSize = fromIntegral windowSize / 8

drawBoard :: UIState -> Picture
drawBoard (UIState gs mode) =
  pictures [
    drawCells,
    drawPieces (board gs),
    drawSelectedPiece (selectedPiece gs),
    drawGameStatus gs,
    drawTurnIndicator gs
  ]

drawCells :: Picture
drawCells = pictures [
  color (if even (i + j) then makeColorI 76 32 16 255 else makeColorI 245 222 179 255) $
    translate (fromIntegral i * cellSize - fromIntegral windowSize / 2 + cellSize / 2)
              (fromIntegral j * cellSize - fromIntegral windowSize / 2 + cellSize / 2) $
    rectangleSolid cellSize cellSize
  | i <- [0..7], j <- [0..7]
  ]

drawPieces :: Board -> Picture
drawPieces board = pictures [
  drawPiece (fromIntegral x * cellSize - fromIntegral windowSize / 2 + cellSize / 2)
            (fromIntegral y * cellSize - fromIntegral windowSize / 2 + cellSize / 2)
            piece
  | (y, row) <- zip [0..] board,
    (x, Just piece) <- zip [0..] row
  ]

drawPiece :: Float -> Float -> Piece -> Picture
drawPiece x y (Piece player pieceType) =
  translate x y $
  color (if player == Black then black else white) $
  pictures [
    circleSolid (cellSize * 0.4),
    color (if player == Black then white else black) $
    if pieceType == King
      then rectangleSolid (cellSize * 0.2) (cellSize * 0.2)
      else blank
  ]

drawSelectedPiece :: Maybe Position -> Picture
drawSelectedPiece Nothing = blank
drawSelectedPiece (Just (x, y)) =
  translate (fromIntegral x * cellSize - fromIntegral windowSize / 2 + cellSize / 2)
            (fromIntegral y * cellSize - fromIntegral windowSize / 2 + cellSize / 2) $
  color yellow $
  rectangleWire cellSize cellSize

drawGameStatus :: GameState -> Picture
drawGameStatus gs
  | isGameOver gs = translate (-100) 0 $ scale 0.2 0.2 $ color red $ 
      text $ "Game Over! " ++ show (oppositePlayer (currentPlayer gs)) ++ " wins!"
  | otherwise = blank

drawTurnIndicator :: GameState -> Picture
drawTurnIndicator gs =
  translate (-fromIntegral windowSize / 2 + 10) (fromIntegral windowSize / 2 - 30) $
  scale 0.15 0.15 $
  color (if currentPlayer gs == Black then black else white) $
  text $ show (currentPlayer gs) ++ "'s Turn"

handleEvent :: Event -> UIState -> UIState
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) uiState@(UIState gs mode) =
  let boardX = floor ((x + fromIntegral windowSize / 2) / cellSize)
      boardY = floor ((y + fromIntegral windowSize / 2) / cellSize)
      pos = (boardX, boardY)
  in if isGameOver gs
    then uiState  -- Prevent moves after game is over
    else case selectedPiece gs of
      Nothing -> 
        if isOwnPiece gs pos
          then UIState (gs { selectedPiece = Just pos }) mode
          else uiState
      Just from ->
        if isValidMove gs from pos
          then case mode of
            SinglePlayer diff -> 
              let playerGs = makeMove gs from pos
              in if isGameOver playerGs
                then UIState playerGs mode
                else UIState (makeAIMove diff playerGs) mode
            TwoPlayer ->
              UIState (makeMove gs from pos) mode
        else UIState (gs { selectedPiece = Just pos }) mode

handleEvent (EventKey (Char 'r') Down _ _) (UIState _ mode) =
  UIState initialGameState mode  -- Reset game
handleEvent _ uiState = uiState

isOwnPiece :: GameState -> Position -> Bool
isOwnPiece (GameState board player _) (x, y) =
  case board !! y !! x of
    Just (Piece p _) -> p == player
    _ -> False

runGame :: GameMode -> IO ()
runGame mode = play (InWindow "Checkers" (windowSize, windowSize) (10, 10))
               white
               60
               (UIState initialGameState mode)
               drawBoard
               handleEvent
               (const id)