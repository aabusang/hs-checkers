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

-- | Convert screen coordinates to board position
screenToBoardPosition :: UIScreenPos -> Maybe UIPosition
screenToBoardPosition (screenX, screenY) =
    let config = boardConfig
        sqSize = squareSize config * scaleFactor config
        (centerX, centerY) = getBoardCenterOffset config
        
        -- Convert screen coordinates relative to board's top-left
        relX = screenX - centerX
        relY = screenY - centerY
        
        -- Convert to board coordinates (0-7)
        row = floor (relY / sqSize)
        col = floor (relX / sqSize)
        
        pos = (row, col)
    in if isValidBoardPosition pos
       then Just pos
       else Nothing

-- | Convert board position to screen coordinates
boardToScreenPosition :: BoardConfig -> UIPosition -> UIScreenPos
boardToScreenPosition config (row, col) =
    let sqSize = squareSize config * scaleFactor config
        (centerX, centerY) = getBoardCenterOffset config
        -- Convert board coordinates to screen coordinates
        screenX = centerX + (fromIntegral col * sqSize) + (sqSize / 2)
        screenY = centerY + (fromIntegral row * sqSize) + (sqSize / 2)
    in (screenX, screenY)

-- | Get board dimensions in pixels
getBoardPixelSize :: BoardConfig -> Float
getBoardPixelSize config = 8 * squareSize config * scaleFactor config

-- | Get the center offset for the entire board
getBoardCenterOffset :: BoardConfig -> (Float, Float)
getBoardCenterOffset config =
    let boardSize = getBoardPixelSize config
    in (offsetX config - boardSize/2, offsetY config - boardSize/2)  -- Center the board in window with offsets