-- |
-- Module      : UI.Types
-- Description : UI-specific types and type aliases

module UI.Types 
    ( -- * Types
      UIPosition      -- ^ Position on the board (0-7, 0-7)
    , UIScreenPos     -- ^ Screen coordinates
    , UIGameState(..) -- ^ Game state from UI perspective
    , UIState(..)     -- ^ Complete UI state
    , UIPiece(..)     -- ^ UI piece representation
    , UIPlayer(..)    -- ^ UI player representation
    , UIPieceType(..) -- ^ UI piece type representation
    ) where

-- | Board position type (0-7, 0-7)
type UIPosition = (Int, Int)

-- | Screen coordinates (Float for precise mouse position)
type UIScreenPos = (Float, Float)

-- | UI-specific piece representation
data UIPiece = UIPiece
    { uiPlayer :: UIPlayer
    , uiPieceType :: UIPieceType
    } deriving (Show)

-- | Players from UI perspective
data UIPlayer = UIBlack | UIWhite
    deriving (Show, Eq)

-- | Piece types from UI perspective
data UIPieceType = UIMan | UIKing
    deriving (Show, Eq)

-- | Simplified game state for UI
data UIGameState = UIGameState
    { uiBoard :: [[Maybe UIPiece]]  -- ^ Board state
    , uiCurrentPlayer :: UIPlayer   -- ^ Current player
    } deriving (Show)

-- | Complete UI state
data UIState = UIState
    { gameState :: UIGameState
    , selectedPosition :: Maybe UIPosition
    , hoverPosition :: Maybe UIPosition  -- ^ Currently hovered board position
    , lastCapture :: Maybe UIPosition
    , captureAnimation :: Float
    } deriving (Show)
