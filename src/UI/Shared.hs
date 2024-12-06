-- | Module for shared UI constants and coordinate handling
module UI.Shared 
    ( BoardConfig(..)
    , defaultConfig
    , screenToBoardPosition
    , boardToScreenPosition
    ) where

import Types.Common (Position, ScreenPosition)
import Board.Validation (isValidBoardPosition)

-- | Configuration for board rendering and input
data BoardConfig = BoardConfig 
    { squareSize :: Float      -- ^ Size of each board square in pixels
    , scaleFactor :: Float     -- ^ Global scale factor for UI
    , offsetX    :: Float      -- ^ X offset from window edge in pixels
    , offsetY    :: Float      -- ^ Y offset from window edge in pixels
    , boardStart :: Int        -- ^ Starting coordinate for board (usually 0)
    , boardEnd   :: Int        -- ^ Ending coordinate for board (usually 7)
    , snapTolerance :: Float   -- ^ How close to a square boundary to snap (0.0 to 1.0)
    }

-- | Default configuration values
defaultConfig :: BoardConfig
defaultConfig = BoardConfig 
    { squareSize = 75          -- Each square is 75 pixels
    , scaleFactor = 1.0        -- No scaling
    , offsetX    = 275         -- Center horizontally 
    , offsetY    = 275         -- Center vertically
    , boardStart = 0           -- Board starts at 0
    , boardEnd   = 7           -- Board ends at 7 (8x8 board)
    , snapTolerance = 0.3      -- Snap when within 30% of square boundary
    }

-- | Scale a measurement based on the config's scale factor
applyScale :: BoardConfig -> Float -> Float
applyScale config value = value * scaleFactor config

-- | Get the scaled square size
getScaledSquareSize :: BoardConfig -> Float
getScaledSquareSize config = applyScale config (squareSize config)

-- | Get scaled offsets
getScaledOffsets :: BoardConfig -> (Float, Float)
getScaledOffsets config = 
    ( applyScale config (offsetX config)
    , applyScale config (offsetY config)
    )

-- | Convert raw screen coordinates to relative board coordinates
screenToRelative :: BoardConfig -> ScreenPosition -> (Float, Float)
screenToRelative config (x, y) =
    let scaledSquare = getScaledSquareSize config
        (scaledX, scaledY) = getScaledOffsets config
    in ( (x + scaledX) / scaledSquare
       , (y + scaledY) / scaledSquare
       )

-- | Convert relative coordinates to board position, handling snap-to-grid
relativeToBoard :: BoardConfig -> (Float, Float) -> Position
relativeToBoard config (relX, relY) =
    let x = floor relX
        lastRow = boardEnd config
        y = floor (fromIntegral lastRow - relY)  -- Simplified y calculation
    in (x, y)

-- | Convert screen coordinates to board position
screenToBoardPosition :: BoardConfig -> ScreenPosition -> Maybe Position
screenToBoardPosition config screenPos =
    let boardPos = (relativeToBoard config . screenToRelative config) screenPos
    in if isValidBoardPosition boardPos
       then Just boardPos
       else Nothing

-- | Convert board coordinates to relative screen position
boardToRelative :: BoardConfig -> Position -> (Float, Float)
boardToRelative config (x, y) =
    ( fromIntegral x
    , fromIntegral (boardEnd config - y)
    )

-- | Convert relative position to screen coordinates
relativeToScreen :: BoardConfig -> (Float, Float) -> ScreenPosition
relativeToScreen config (relX, relY) =
    let scaledSquare = getScaledSquareSize config
        (scaledX, scaledY) = getScaledOffsets config
    in ( relX * scaledSquare - scaledX
       , relY * scaledSquare - scaledY
       )

-- | Convert board position to screen coordinates
boardToScreenPosition :: BoardConfig -> Position -> ScreenPosition
boardToScreenPosition config = 
    relativeToScreen config . boardToRelative config