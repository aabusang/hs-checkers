-- |
-- Module      : UI.Types
-- Description : UI-specific types and type aliases

module UI.Types 
    ( -- * Types
      UIPosition      -- ^ Position on the board (0-7, 0-7)
    , UIScreenPos     -- ^ Screen coordinates (Float for precise mouse position)
    , UIPlayer(..)    -- ^ Players from UI perspective
    , UIPieceType(..) -- ^ Piece types from UI perspective
    , UIPiece(..)     -- ^ UI-specific piece representation
    , UIGameState(..) -- ^ Simplified game state for UI
    , UIState(..)     -- ^ Complete UI state
    ) where

import Game.Mode (GameMode(..))

-- | Board position type (0-7, 0-7)
type UIPosition = (Int, Int)

-- | Screen coordinates (Float for precise mouse position)
type UIScreenPos = (Float, Float)

-- | Players from UI perspective
data UIPlayer = UIBlack | UIWhite
    deriving (Show, Eq)

-- | Piece types from UI perspective
data UIPieceType = UIMan | UIKing
    deriving (Show, Eq)

-- | UI-specific piece representation
data UIPiece = UIPiece
    { uiPlayer :: UIPlayer
    , uiPieceType :: UIPieceType
    } deriving (Show, Eq)

-- | The game board
type UIBoard = [[Maybe UIPiece]]

-- | Simplified game state for UI
data UIGameState = UIGameState
    { uiBoard :: UIBoard  -- ^ Board state
    , uiCurrentPlayer :: UIPlayer   -- ^ Current player
    } deriving (Show)

-- | Complete UI state
data UIState = UIState
    { gameState :: UIGameState
    , selectedPosition :: Maybe UIPosition
    , hoverPosition :: Maybe UIPosition  -- ^ Currently hovered board position
    , lastCapture :: Maybe UIPosition
    , captureAnimation :: Float
    , gameMode :: GameMode              -- ^ Current game mode
    } deriving (Show)
