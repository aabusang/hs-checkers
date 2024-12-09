module UI.Board 
    ( drawBoard
    , drawSquare
    , squareColor
    ) where

import Graphics.Gloss
import UI.Types
import UI.Shared (boardToScreenPosition)
import UI.Config (BoardConfig(..), boardConfig)

drawBoard :: Picture
drawBoard = pictures [drawSquares]

drawSquare :: UIPosition -> Picture
drawSquare (row, col) =
    let squareWidth = squareSize boardConfig * scaleFactor boardConfig
        (screenX, screenY) = boardToScreenPosition boardConfig (row, col)
    in translate screenX screenY $
       pictures [
           color (squareColor (row, col)) $
           rectangleSolid squareWidth squareWidth,
           color (greyN 0.3) $
           rectangleWire squareWidth squareWidth
       ]

squareColor :: UIPosition -> Color
squareColor (row, col) = 
    if even (row + col)
    then makeColorI 89 62 49 255     -- Dark wood (mahogany)
    else makeColorI 181 136 99 255   -- Light wood (beige brown)

drawSquares :: Picture
drawSquares =
    pictures
        [ drawSquare (row, col)
        | row <- [0..7]  -- Use fixed board size
        , col <- [0..7]
        ]