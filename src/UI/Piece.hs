-- |
-- Module      : UI.Piece
-- Description : Piece rendering and configuration
module UI.Piece
    ( drawPieces
    ) where

import Graphics.Gloss
import UI.Types
import UI.Shared (boardToScreenPosition)
import UI.Config (BoardConfig(..), boardConfig, PieceConfig(..), defaultPieceConfig)

-- | Get the color for a piece
pieceColor :: UIPiece -> Bool -> Color
pieceColor piece isSelected =
    let baseColor = case uiPlayer piece of
            UIBlack -> makeColorI 40 40 40 255      -- Dark grey/black
            UIWhite -> makeColorI 245 245 245 255   -- Off-white
    in if isSelected then light yellow else baseColor

-- | Draw a single piece
drawPiece :: UIPosition -> UIPiece -> Bool -> Picture
drawPiece pos piece isSelected =
    let config = boardConfig
        (x, y) = boardToScreenPosition config pos
        radius = squareSize config * scaleFactor config * pieceScale defaultPieceConfig * 0.45
        mainPiece = pictures [
            translate x y $ color (pieceColor piece isSelected) $ circleSolid radius,
            translate x y $ color (greyN 0.3) $ circleSolid radius
            ]
        crown = if uiPieceType piece == UIKing
               then translate x y $ color (dark yellow) $ 
                    scale 0.5 0.5 $ 
                    pictures [circleSolid (radius * 0.5),
                             translate 0 (radius * 0.3) $ circleSolid (radius * 0.2)]
               else blank
    in pictures [mainPiece, crown]

-- | Draw all pieces on the board
drawPieces :: [[Maybe UIPiece]] -> Maybe UIPosition -> [Picture]
drawPieces board maybeSelected =
    [ drawPiece (x, y) piece (maybeSelected == Just (x, y))
    | x <- [0..7]
    , y <- [0..7]
    , Just piece <- [board !! y !! x]
    ]