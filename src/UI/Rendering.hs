-- |
-- Module      : UI.Rendering
-- Description : Rendering functions for the Checkers game
-- 
-- This module handles all the graphical rendering using the Gloss library,
-- including drawing the board, pieces, and game state.

module UI.Rendering
    ( -- * Window Settings
      windowWidth
    , windowHeight
    , squareSize
      -- * Drawing Functions
    , drawGameState
    , drawBoard
    , drawPiece
    ) where

import Graphics.Gloss
import Board.Types (Player(..), Piece(..), Board)
import Game.State (GameState(..))
import UI.Types (UIState(..))

-- | Window settings
windowWidth, windowHeight :: Int
windowWidth = 800
windowHeight = 800

-- | Square size in pixels
squareSize :: Float
squareSize = 50

-- | Draws a checker piece
drawPiece :: Piece -> Picture
drawPiece (Piece player pieceType') =
    color pieceColor $ pictures [circleSolid radius]
  where
    pieceColor = case player of
        Black -> black
        White -> white
    radius = 20

-- | Draws the game board
drawBoard :: Board -> Picture
drawBoard board' =
    pictures [squares, pieces]
  where
    squares = pictures
        [ translate (fromIntegral x * squareSize) (fromIntegral y * squareSize) $
          color (if even (x + y) then light (greyN 0.5) else dark (greyN 0.5)) $
          rectangleSolid squareSize squareSize
        | x <- [0..7 :: Int]
        , y <- [0..7 :: Int]
        ]
    pieces = pictures
        [ translate (fromIntegral x * squareSize) (fromIntegral y * squareSize) $
          maybe blank drawPiece (board' !! y !! x)
        | x <- [0..7]
        , y <- [0..7]
        ]

-- | Draws the game state
drawGameState :: UIState -> Picture
drawGameState (UIState gs _) =
    pictures [translate (-200) (-200) $ drawBoard (board gs)]
