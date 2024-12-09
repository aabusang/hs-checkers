-- |
-- Module      : UI.Conversion
-- Description : Conversions between UI and game types
module UI.Conversion
    ( toUIState
    , fromUIState
    , toUIPosition
    , fromUIPosition
    , toUIPiece
    , fromUIPiece
    ) where

import qualified Types.Game as G
import qualified Types.Common as C
import qualified Board.Types as B
import qualified UI.Types as UI

-- | Convert game state to UI state
toUIState :: G.GameState -> UI.UIState
toUIState gs = UI.UIState
    { UI.gameState = UI.UIGameState
        { UI.uiBoard = map (map (fmap toUIPiece)) (G.board gs)
        , UI.uiCurrentPlayer = toUIPlayer (G.currentPlayer gs)
        }
    , UI.selectedPosition = Nothing
    , UI.hoverPosition = Nothing
    , UI.lastCapture = Nothing
    , UI.captureAnimation = 0.0
    }

-- | Convert UI state to game state
fromUIState :: UI.UIState -> G.GameState
fromUIState uiState = G.GameState
    { G.board = map (map (fmap fromUIPiece)) (UI.uiBoard $ UI.gameState uiState)
    , G.currentPlayer = fromUIPlayer (UI.uiCurrentPlayer $ UI.gameState uiState)
    , G.selectedPiecePos = fromUIPosition <$> UI.selectedPosition uiState
    , G.validMoves = []  -- Will be recalculated by game logic
    , G.gameStatus = G.Ongoing
    , G.movesSinceCapture = 0  -- Reset on state conversion
    }

-- | Convert game position to UI position
toUIPosition :: C.Position -> UI.UIPosition
toUIPosition = id

-- | Convert UI position to game position
fromUIPosition :: UI.UIPosition -> C.Position
fromUIPosition = id

-- | Convert game piece to UI piece
toUIPiece :: B.Piece -> UI.UIPiece
toUIPiece (B.Piece player pieceType) = UI.UIPiece
    { UI.uiPlayer = toUIPlayer player
    , UI.uiPieceType = toUIPieceType pieceType
    }

-- | Convert UI piece to game piece
fromUIPiece :: UI.UIPiece -> B.Piece
fromUIPiece piece = B.Piece
    { B.pieceOwner = fromUIPlayer (UI.uiPlayer piece)
    , B.pieceType = fromUIPieceType (UI.uiPieceType piece)
    }

-- | Convert game player to UI player
toUIPlayer :: B.Player -> UI.UIPlayer
toUIPlayer B.Black = UI.UIBlack
toUIPlayer B.White = UI.UIWhite

-- | Convert UI player to game player
fromUIPlayer :: UI.UIPlayer -> B.Player
fromUIPlayer UI.UIBlack = B.Black
fromUIPlayer UI.UIWhite = B.White

-- | Convert game piece type to UI piece type
toUIPieceType :: B.PieceType -> UI.UIPieceType
toUIPieceType B.Man = UI.UIMan
toUIPieceType B.King = UI.UIKing

-- | Convert UI piece type to game piece type
fromUIPieceType :: UI.UIPieceType -> B.PieceType
fromUIPieceType UI.UIMan = B.Man
fromUIPieceType UI.UIKing = B.King
