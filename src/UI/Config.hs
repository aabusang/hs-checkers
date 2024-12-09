-- |
-- Module      : UI.Config
-- Description : UI configuration settings
module UI.Config 
    ( -- * Board Configuration
      BoardConfig(..)
    , defaultBoardConfig
    , boardConfig  
      -- * Piece Configuration
    , PieceConfig(..)
    , defaultPieceConfig
      -- * Window Configuration
    , windowWidth
    , windowHeight
    , windowPosX
    , windowPosY
    , fps
    ) where

-- | Configuration for board rendering and input
data BoardConfig = BoardConfig 
    { squareSize :: Float      -- ^ Size of each board square in pixels
    , scaleFactor :: Float     -- ^ Global scale factor for UI
    , offsetX    :: Float      -- ^ X offset from window edge in pixels
    , offsetY    :: Float      -- ^ Y offset from window edge in pixels
    , boardStart :: Int        -- ^ Starting coordinate for board (usually 0)
    , boardEnd   :: Int        -- ^ Ending coordinate for board (usually 7)
    -- , snapTolerance :: Float   -- ^ How close to a square boundary to snap (0.0 to 1.0)
    }

-- | Default configuration values
defaultBoardConfig :: BoardConfig
defaultBoardConfig = BoardConfig 
    { squareSize = 80          -- Each square is 80 pixels (for 8x8 board)
    , scaleFactor = 1.0        -- No scaling
    , offsetX    = 50
    , offsetY    = 50
    , boardStart = 0         -- Board starts at 0
    , boardEnd   = 7           -- Board ends at 7 (8x8 board)
    -- , snapTolerance = 0.3      -- Snap when within 30% of square boundary
    }

-- | The active board configuration
-- This allows us to potentially modify the config at runtime
boardConfig :: BoardConfig
boardConfig = defaultBoardConfig

-- | Configuration for piece rendering
data PieceConfig = PieceConfig
    { pieceScale :: Float      -- ^ Size of piece relative to square (0.0 to 1.0)
    , highlightScale :: Float  -- ^ Size of highlight relative to piece (> 1.0)
    }

-- | Default piece configuration
defaultPieceConfig :: PieceConfig
defaultPieceConfig = PieceConfig
    { pieceScale = 0.7
    , highlightScale = 1.2
    }

-- | Window dimensions
windowWidth, windowHeight :: Int
windowWidth = 1000
windowHeight = 800

-- | Window position on screen
windowPosX, windowPosY :: Int
windowPosX = 600  -- Initial X position of window on screen
windowPosY = 100  -- Initial Y position of window on screen

-- | Frames per second
fps :: Int
fps = 60
