{-|
Module      : UI
Description : User interface for the Checkers game
Copyright   : (c) Your Name, 2024
License     : Your License

This module handles the graphical user interface for the Checkers game using
the Gloss library. It includes functions for:
  * Drawing the game board
  * Handling user input
  * Displaying game state
-}
module UI
    ( -- * UI Functions
      playCheckers
    ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Types (Player(..), PieceType(..), Piece(..), Position, Board, GameState(..), GameMode(..), currentPlayer, selectedPiece, board, GameMode(..))
import Game (initialGameState)
import AI (makeAIMove)
import Rules (isValidMove, isGameOver, makeMove, oppositePlayer)

-- | Window settings
windowWidth, windowHeight :: Int
windowWidth = 800
windowHeight = 800

-- | Square size in pixels
squareSize :: Float
squareSize = 50

-- | The state of the UI
data UIState = UIState
    { gameState :: GameState  -- ^ Current game state
    , gameMode :: GameMode    -- ^ Current game mode (PvP or PvAI)
    }

-- | Initial UI state
initialUIState :: GameMode -> UIState
initialUIState mode = UIState initialGameState mode

-- | Draws a checker piece
drawPiece :: Piece -> Picture
drawPiece (Piece player pieceType) =
    let baseColor = case player of
            Black -> black
            White -> white
        kingMark = case pieceType of
            King -> pictures [color (opposite baseColor) $ circleSolid 10]
            Man -> blank
        opposite black = white
        opposite white = black
    in pictures [ color baseColor $ circleSolid 20
               , kingMark
               ]

-- | Draws the game board
drawBoard :: Board -> Picture
drawBoard board =
    pictures [ squares
             , pieces
             ]
  where
    squares = pictures
        [ color (if even (i + j) then dark brown else light brown) $
          translate (fromIntegral i * squareSize - offset)
                   (fromIntegral j * squareSize - offset) $
          rectangleSolid squareSize squareSize
        | i <- [0..7]
        , j <- [0..7]
        ]
    pieces = pictures
        [ translate (fromIntegral i * squareSize - offset)
                   (fromIntegral j * squareSize - offset) $
          maybe blank drawPiece piece
        | i <- [0..7]
        , j <- [0..7]
        , let piece = board !! j !! i
        ]
    offset = fromIntegral (windowWidth `div` 2) - squareSize * 4
    brown = makeColor 0.6 0.4 0.2 1.0
    dark c = makeColor (r*0.8) (g*0.8) (b*0.8) a
      where (r,g,b,a) = rgbaOfColor c
    light c = makeColor (r*1.2) (g*1.2) (b*1.2) a
      where (r,g,b,a) = rgbaOfColor c

-- | Draws the game state
drawGameState :: UIState -> Picture
drawGameState (UIState gs mode)
  | isGameOver gs = translate (-100) 0 $ scale 0.2 0.2 $ color red $ 
      text $ "Game Over! " ++ show (oppositePlayer (currentPlayer gs)) ++ " wins!"
  | otherwise = pictures [ drawBoard (board gs)
                        , translate (-100) (-350) $ scale 0.2 0.2 $ 
                          text $ show (currentPlayer gs) ++ "'s turn"
                        ]

-- | Converts screen coordinates to board coordinates
screenToBoard :: (Float, Float) -> Maybe Position
screenToBoard (x, y) =
    let offset = fromIntegral (windowWidth `div` 2) - squareSize * 4
        i = floor ((x + offset) / squareSize)
        j = floor ((y + offset) / squareSize)
    in if i >= 0 && i < 8 && j >= 0 && j < 8
       then Just (i, j)
       else Nothing

-- | Handles mouse input
handleInput :: Event -> UIState -> UIState
handleInput (EventKey (MouseButton LeftButton) Down _ (x, y)) (UIState gs mode) =
  let pos = screenToBoard (x, y)
  in if isGameOver gs
     then UIState initialGameState mode  -- Start a new game when game is over
     else case pos of
          Nothing -> UIState gs mode
          Just pos -> case selectedPiece gs of
            Nothing -> case board gs !! snd pos !! fst pos of
              Just (Piece p _) | p == currentPlayer gs ->
                UIState (gs { selectedPiece = Just pos }) mode
              _ -> UIState gs mode
            Just from ->
              if isValidMove gs from pos
              then let playerGs = makeMove gs from pos
                   in if isGameOver playerGs
                      then UIState playerGs mode  -- Game over after player's move
                      else case mode of
                           SinglePlayer _ -> UIState playerGs mode  -- AI will move in update
                           TwoPlayer -> UIState playerGs mode
              else UIState (gs { selectedPiece = Nothing }) mode
handleInput _ state = state

-- | Updates the game state
updateGame :: Float -> UIState -> UIState
updateGame _ state@(UIState gs mode)
  | case mode of
      SinglePlayer _ -> currentPlayer gs == Black && not (isGameOver gs)
      TwoPlayer -> False =
      state  -- AI move will be made in event handling
  | otherwise = state

-- | Starts the game
playCheckers :: GameMode -> IO ()
playCheckers mode = play window background fps (initialUIState mode)
                        drawGameState handleInput updateGame
  where
    window = InWindow "Checkers" (windowWidth, windowHeight) (10, 10)
    background = white
    fps = 30