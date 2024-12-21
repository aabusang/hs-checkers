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
    , toUIPlayer
    , fromUIPlayer
    , toUIPieceType
    , fromUIPieceType
    ) where

import qualified UI.Types as UI
import qualified Types.Game as Game
import qualified Board.Types as Board
import qualified Game.Mode as Game (GameMode(..))
import qualified Types.Common as Common
import Rules.Movement (getValidMoves)

-- | Convert game state to UI state
toUIState :: Game.GameState -> UI.UIState
toUIState gameState = UI.UIState
    { UI.gameState = UI.UIGameState
        { UI.uiBoard = map (map (fmap toUIPiece)) (Game.board gameState)
        , UI.uiCurrentPlayer = toUIPlayer (Game.currentPlayer gameState)
        }
    , UI.selectedPosition = Nothing
    , UI.hoverPosition = Nothing
    , UI.lastCapture = Nothing
    , UI.captureAnimation = 0.0
    , UI.gameMode = Game.TwoPlayer
    }

-- | Convert UI state to game state
fromUIState :: UI.UIState -> Game.GameState
fromUIState uiState = Game.GameState
    { Game.board = map (map (fmap fromUIPiece)) (UI.uiBoard $ UI.gameState uiState)
    , Game.currentPlayer = fromUIPlayer (UI.uiCurrentPlayer $ UI.gameState uiState)
    , Game.selectedPiecePos = UI.selectedPosition uiState >>= \pos -> Just (fromUIPosition pos)
    , Game.validMoves = calculateValidMoves uiState
    , Game.gameStatus = Game.Ongoing
    , Game.movesSinceCapture = 0
    }

-- | Calculate valid moves for the current state
calculateValidMoves :: UI.UIState -> [Common.Position]
calculateValidMoves uiState =
    case UI.selectedPosition uiState of
        Nothing -> []
        Just pos -> getValidMoves (fromUIState uiState) (fromUIPosition pos)

-- | Convert a piece from game to UI representation
toUIPiece :: Board.Piece -> UI.UIPiece
toUIPiece (Board.Piece player pieceType) = UI.UIPiece
    { UI.uiPlayer = toUIPlayer player
    , UI.uiPieceType = toUIPieceType pieceType
    }

-- | Convert a piece from UI to game representation
fromUIPiece :: UI.UIPiece -> Board.Piece
fromUIPiece piece = Board.Piece
    { Board.pieceOwner = fromUIPlayer (UI.uiPlayer piece)
    , Board.pieceType = fromUIPieceType (UI.uiPieceType piece)
    }

-- | Convert a position from game to UI representation
toUIPosition :: Common.Position -> UI.UIPosition
toUIPosition pos = pos  -- They're the same type, just different names

-- | Convert a position from UI to game representation
fromUIPosition :: UI.UIPosition -> Common.Position
fromUIPosition pos = pos  -- They're the same type, just different names

-- | Convert a player from game to UI representation
toUIPlayer :: Board.Player -> UI.UIPlayer
toUIPlayer Board.Black = UI.UIBlack
toUIPlayer Board.White = UI.UIWhite

-- | Convert a player from UI to game representation
fromUIPlayer :: UI.UIPlayer -> Board.Player
fromUIPlayer UI.UIBlack = Board.Black
fromUIPlayer UI.UIWhite = Board.White

-- | Convert a piece type from game to UI representation
toUIPieceType :: Board.PieceType -> UI.UIPieceType
toUIPieceType Board.Man = UI.UIMan
toUIPieceType Board.King = UI.UIKing

-- | Convert a piece type from UI to game representation
fromUIPieceType :: UI.UIPieceType -> Board.PieceType
fromUIPieceType UI.UIMan = Board.Man
fromUIPieceType UI.UIKing = Board.King
