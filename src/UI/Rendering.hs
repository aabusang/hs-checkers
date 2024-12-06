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

-- Game types
import Board.Types (Player(..), Piece(..), Board)
import Types.Common (Position)
import Game.State (GameState(..))

-- UI types and configuration
import UI.Types (UIState(..))
import UI.Shared 
    ( BoardConfig(..)
    , defaultConfig
    , boardToScreenPosition
    )

-- | Window settings
windowWidth, windowHeight :: Int
windowWidth = 800
windowHeight = 800

-- | Piece rendering configuration
data PieceConfig = PieceConfig
    { pieceScale :: Float  -- ^ Size of piece relative to square (0.0 to 1.0)
    , highlightScale :: Float  -- ^ Size of highlight relative to piece (> 1.0)
    }

-- | Default piece configuration
defaultPieceConfig :: PieceConfig
defaultPieceConfig = PieceConfig
    { pieceScale = 0.4    -- Piece takes up 40% of square
    , highlightScale = 1.2  -- Highlight is 20% larger than piece
    }

-- | Draw the checkered board squares
drawSquares :: BoardConfig -> Picture
drawSquares config =
    pictures
        [ translate screenX screenY $
          color squareColor $
          rectangleSolid squareWidth squareWidth
        | x <- [boardStart config..boardEnd config]
        , y <- [boardStart config..boardEnd config]
        , let squareWidth = squareSize config * scaleFactor config
        , let (screenX, screenY) = boardToScreenPosition config (x, y)
        , let squareColor = if odd (x + y) 
                           then light (greyN 0.5)  -- Light squares
                           else dark (greyN 0.5)   -- Dark squares
        ]

-- | Draw a single piece
drawPiece :: BoardConfig -> PieceConfig -> Maybe Position -> Position -> Piece -> Picture
drawPiece boardConfig pieceConfig maybeSelected pos (Piece player _) =
    let (x, y) = boardToScreenPosition boardConfig pos
        isSelected = maybeSelected == Just pos
        baseColor = if player == Black then light red else dark blue
        pieceColor = if isSelected then light yellow else baseColor
        radius = squareSize boardConfig * scaleFactor boardConfig * pieceScale pieceConfig
    in translate x y $ color pieceColor $ circleSolid radius

-- | Draw all pieces on the board
drawPieces :: BoardConfig -> PieceConfig -> Board -> Maybe Position -> [Picture]
drawPieces boardConfig pieceConfig inputBoard maybeSelected =
    [ drawPiece boardConfig pieceConfig maybeSelected (x, y) piece
    | x <- [0..7]
    , y <- [0..7]
    , Just piece <- [inputBoard !! y !! x]
    ]

-- | Draw the complete game state
drawGameState :: UIState -> Picture
drawGameState (UIState gs selectedPos) =
    pictures
    [ drawSquares defaultConfig
    , pictures $ drawPieces defaultConfig defaultPieceConfig (board gs) selectedPos
    ]