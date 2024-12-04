-- | Module for shared UI constants and coordinate handling
module UI.Shared 
    ( BoardConfig(..)
    , defaultConfig
    , ScreenPosition
    , BoardPosition
    , screenToBoardPosition
    , boardToScreenPosition
    ) where

import UI.Types (BoardPosition, ScreenPosition)


-- | Configuration for board rendering and input
data BoardConfig = BoardConfig 
    { squareSize :: Float      -- Size of each board square
    , scaleFactor :: Float     -- Global scale factor for UI
    , offsetX    :: Float      -- X offset from window edge
    , offsetY    :: Float      -- Y offset from window edge
    , boardStart :: Int        -- Starting coordinate for board
    , boardEnd   :: Int        -- Ending coordinate for board
    }

-- | Default configuration values
defaultConfig :: BoardConfig
defaultConfig = BoardConfig 
    { squareSize = 75
    , scaleFactor = 1.0
    , offsetX    = -250
    , offsetY    = -250
    , boardStart = 0
    , boardEnd   = 7
    }

-- | Convert screen coordinates to board position
screenToBoardPosition :: BoardConfig -> ScreenPosition -> Maybe BoardPosition
screenToBoardPosition config (x, y) =
    let scaledSquareSize = squareSize config * scaleFactor config
        scaledOffsetX = offsetX config * scaleFactor config
        scaledOffsetY = offsetY config * scaleFactor config
        rawX = (x - scaledOffsetX) / scaledSquareSize
        rawY = (y - scaledOffsetY) / scaledSquareSize
        boardX = floor rawX
        boardY = 7 - floor rawY  -- Flip the y-coordinate to match the board array
    in if isValidBoardPosition (boardX, boardY)
       then Just (boardX, boardY)
       else Nothing

-- | Convert board position to screen coordinates
boardToScreenPosition :: BoardConfig -> BoardPosition -> ScreenPosition
boardToScreenPosition config (x, y) =
    let scaledSquareSize = squareSize config * scaleFactor config
        scaledOffsetX = offsetX config * scaleFactor config
        scaledOffsetY = offsetY config * scaleFactor config
        screenX = fromIntegral x * scaledSquareSize + scaledOffsetX
        screenY = fromIntegral y * scaledSquareSize + scaledOffsetY
    in (screenX, screenY)

-- | Check if a position is within the board boundaries
isValidBoardPosition :: BoardPosition -> Bool
isValidBoardPosition (x, y) = x >= 0 && x < 8 && y >= 0 && y < 8