-- |
-- Module      : UI.Rendering
-- Description : Rendering functions for the Checkers game
-- 
-- This module handles all the graphical rendering using the Gloss library,
-- including drawing the board, pieces, and game state.
module UI.Rendering
    ( drawGameState
    , windowWidth
    , windowHeight
    ) where

import Graphics.Gloss
import Board.Types (Player(..), Piece(..), Board, Position)
import Game.State (GameState(..))
import UI.Types (UIState(..))
import UI.Shared (BoardConfig(..), defaultConfig, boardToScreenPosition)

-- | Window settings
windowWidth, windowHeight :: Int
windowWidth = 800
windowHeight = 800

-- | Draw the checkered board squares
drawSquares :: BoardConfig -> Picture
drawSquares config =
    pictures
        [ translate screenX screenY $
          color squareColor $
          rectangleSolid scaledSquareSize scaledSquareSize
        | x <- [boardStart config..boardEnd config]
        , y <- [boardStart config..boardEnd config]
        , let scaledSquareSize = squareSize config * scaleFactor config
        , let (screenX, screenY) = boardToScreenPosition config (x, y)
        , let squareColor = if odd (x + y) 
                           then light (greyN 0.5)
                           else dark (greyN 0.5)
        ]

-- | Draw a single piece
drawPiece :: Maybe Position -> Position -> Piece -> Picture
drawPiece selectedPos pos (Piece player _) =
    let config = defaultConfig
        scaledSquareSize = squareSize config * scaleFactor config
        pieceRadius = scaledSquareSize * 0.4
        baseCircle = color pieceColor $ circleSolid pieceRadius
        highlight = if Just pos == selectedPos
                   then color yellow $ circle (pieceRadius * 1.2)
                   else Blank
        pieceColor = if player == Black then black else white
    in pictures [baseCircle, highlight]

-- | Draw all pieces on the board
drawPieces :: BoardConfig -> Board -> Maybe Position -> Picture
drawPieces boardConfig inputBoard selectedPos =
    pictures
        [ translate screenX screenY $
          case inputBoard !! y !! x of
            Just piece -> drawPiece selectedPos (x, y) piece
            Nothing -> Blank
        | x <- [0..7]
        , y <- [0..7]
        , let (screenX, screenY) = boardToScreenPosition boardConfig (x, y)
        ]

-- | Draw the complete game state
drawGameState :: UIState -> Picture
drawGameState (UIState (GameState inputBoard _ _) _ selectedPos) =
    let config = defaultConfig
    in pictures [ drawSquares config
                , drawPieces config inputBoard selectedPos
                ]