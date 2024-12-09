-- |
-- Module      : UI.Rendering
-- Description : Rendering functions for the Checkers game
-- 
-- This module handles all the graphical rendering using the Gloss library,
-- including drawing the board, pieces, and game state.
module UI.Rendering
    ( -- * Window Configuration
      windowWidth
    , windowHeight
      -- * Rendering
    , drawGameState
    ) where

-- External imports
import Graphics.Gloss

-- UI types and configuration
import UI.Types
import UI.Config (BoardConfig(..), boardConfig, PieceConfig(..), defaultPieceConfig, windowWidth, windowHeight)
import UI.Shared (boardToScreenPosition)
import UI.Board (squareColor)


-- | Get the color for a piece
pieceColor :: UIPiece -> Color
pieceColor piece = case uiPlayer piece of
    UIBlack -> makeColorI 40 40 40 255      -- Dark grey/black
    UIWhite -> makeColorI 245 245 245 255   -- Off-white

-- | Get a piece at a board position
getPieceAt :: UIGameState -> UIPosition -> Maybe UIPiece
getPieceAt gameState (row, col) = (uiBoard gameState) !! row !! col

-- | Draw the checkered board squares
drawSquares :: Picture
drawSquares =
    let config = boardConfig
        squareWidth = squareSize config * scaleFactor config
    in pictures
        [ translate screenPosX screenPosY $
          pictures [
              -- Base square
              color (squareColor (row, col)) $
              rectangleSolid squareWidth squareWidth,
              -- Grid lines for better visibility
              color (greyN 0.3) $
              rectangleWire squareWidth squareWidth
          ]
        | row <- [0..7]
        , col <- [0..7]
        , let (screenPosX, screenPosY) = boardToScreenPosition config (row, col)
        ]

-- | Draw a piece at a board position
drawPiece :: UIPosition -> UIPiece -> Picture
drawPiece pos piece =
    let config = boardConfig
        (screenX, screenY) = boardToScreenPosition config pos
        pieceConfig = defaultPieceConfig
        radius = squareSize config * scaleFactor config * pieceScale pieceConfig / 2
    in translate screenX screenY $
       color (pieceColor piece) $
       circleSolid radius

-- | Draw the game state
drawGameState :: UIState -> Picture
drawGameState state = 
    pictures $
        [ drawSquares
        , drawPieces (gameState state)
        , maybe blank drawHighlight (selectedPosition state)
        ]
  where
    drawPieces gameState = pictures
        [ maybe blank (drawPiece (row, col)) piece
        | row <- [0..7]
        , col <- [0..7]
        , let piece = getPieceAt gameState (row, col)
        ]
    
    drawHighlight position =
        let config = boardConfig
            (screenPosX, screenPosY) = boardToScreenPosition config position
            pieceConfig = defaultPieceConfig
            radius = squareSize config * scaleFactor config * 
                    pieceScale pieceConfig * highlightScale pieceConfig / 2
        in translate screenPosX screenPosY $
           color (makeColor 1 1 0 0.3) $  -- Semi-transparent yellow
           circleSolid radius