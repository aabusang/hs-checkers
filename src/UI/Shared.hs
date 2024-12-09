-- |
-- Module      : UI.Shared
-- Description : Shared UI utilities for coordinate conversion
module UI.Shared 
    ( screenToBoardPosition
    , boardToScreenPosition
    ) where

import UI.Types (UIPosition, UIScreenPos, UIPiece, uiPlayer)
import UI.Config (BoardConfig(..), boardConfig)
import Board.Validation (isValidBoardPosition)
import Debug.Trace (trace)

-- | Get board dimensions in pixels
getBoardPixelSize :: BoardConfig -> Float
getBoardPixelSize config = 8 * squareSize config * scaleFactor config

-- | Get the center offset for the entire board
getBoardCenterOffset :: BoardConfig -> (Float, Float)
getBoardCenterOffset config =
    let boardSize = getBoardPixelSize config
    in (offsetX config - boardSize/2, offsetY config - boardSize/2)  -- Center the board in window with offsets

-- | Convert screen coordinates to board position
screenToBoardPosition :: [[Maybe UIPiece]] -> UIScreenPos -> Maybe UIPosition
screenToBoardPosition board (screenX, screenY) =
    let config = boardConfig
        sqSize = squareSize config * scaleFactor config
        (centerX, centerY) = getBoardCenterOffset config
        
        -- Convert screen coordinates relative to board's top-left
        relX = screenX - centerX
        relY = screenY - centerY
        
        -- Convert to board coordinates (0-7) using round instead of floor
        -- This provides better handling of clicks near square edges
        row = round (relY / sqSize)
        col = round (relX / sqSize)
        
        pos = (row, col)
        
        -- Get piece information for debugging
        pieceInfo = case (if isValidBoardPosition pos 
                         then (board !! row) !! col
                         else Nothing) of
            Just piece -> " Piece: " ++ show (uiPlayer piece)
            Nothing -> " No piece"
        
        -- Debug output
        debug = "Screen: (" ++ show screenX ++ "," ++ show screenY ++ 
                ") Rel: (" ++ show relX ++ "," ++ show relY ++ 
                ") Board: " ++ show pos ++ pieceInfo
    in trace debug $ 
       if isValidBoardPosition pos
       then Just pos
       else Nothing

-- | Convert board position to screen coordinates
boardToScreenPosition :: BoardConfig -> UIPosition -> UIScreenPos
boardToScreenPosition config (x, y) =
    let sqSize = squareSize config * scaleFactor config
        (centerX, centerY) = getBoardCenterOffset config
    in ( fromIntegral x * sqSize + centerX
       , fromIntegral y * sqSize + centerY
       )