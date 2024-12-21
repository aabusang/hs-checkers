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

import qualified Types.Game as Game
import qualified Types.Common as Common
import qualified Board.Types as Board
import qualified UI.Types as UI
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
    }

-- | Convert board from UI to game representation
convertBoard :: [[Maybe UI.UIPiece]] -> Board.Board
convertBoard = map (map (fmap fromUIPiece))

-- | Create a temporary game state for move validation
createTempGameState :: [[Maybe UI.UIPiece]] -> Board.Player -> Game.GameState
createTempGameState board player = Game.GameState
    { Game.board = convertBoard board
    , Game.currentPlayer = player
    , Game.selectedPiecePos = Nothing
    , Game.validMoves = []
    , Game.gameStatus = Game.Ongoing
    , Game.movesSinceCapture = 0
    }

-- | Calculate valid moves for a selected position
calculateValidMoves :: UI.UIState -> [Common.Position]
calculateValidMoves uiState = 
    case UI.selectedPosition uiState of
        Nothing -> []
        Just pos -> 
            let board = UI.uiBoard $ UI.gameState uiState
                player = fromUIPlayer $ UI.uiCurrentPlayer $ UI.gameState uiState
                tempState = createTempGameState board player
            in getValidMoves tempState (fromUIPosition pos)

-- | Convert UI state to game state
fromUIState :: UI.UIState -> Game.GameState
fromUIState uiState = Game.GameState
    { Game.board = convertBoard (UI.uiBoard $ UI.gameState uiState)
    , Game.currentPlayer = fromUIPlayer (UI.uiCurrentPlayer $ UI.gameState uiState)
    , Game.selectedPiecePos = fromUIPosition <$> UI.selectedPosition uiState
    , Game.validMoves = calculateValidMoves uiState
    , Game.gameStatus = Game.Ongoing
    , Game.movesSinceCapture = 0
    }

-- | Convert game position to UI position
toUIPosition :: Common.Position -> UI.UIPosition
toUIPosition = id

-- | Convert UI position to game position
fromUIPosition :: UI.UIPosition -> Common.Position
fromUIPosition = id

-- | Convert game piece to UI piece
toUIPiece :: Board.Piece -> UI.UIPiece
toUIPiece piece = UI.UIPiece
    { UI.uiPlayer = toUIPlayer (Board.pieceOwner piece)
    , UI.uiPieceType = toUIPieceType (Board.pieceType piece)
    }

-- | Convert UI piece to game piece
fromUIPiece :: UI.UIPiece -> Board.Piece
fromUIPiece piece = Board.Piece
    { Board.pieceOwner = fromUIPlayer (UI.uiPlayer piece)
    , Board.pieceType = fromUIPieceType (UI.uiPieceType piece)
    }

-- | Convert game player to UI player
toUIPlayer :: Board.Player -> UI.UIPlayer
toUIPlayer Board.Black = UI.UIBlack
toUIPlayer Board.White = UI.UIWhite

-- | Convert UI player to game player
fromUIPlayer :: UI.UIPlayer -> Board.Player
fromUIPlayer UI.UIBlack = Board.Black
fromUIPlayer UI.UIWhite = Board.White

-- | Convert game piece type to UI piece type
toUIPieceType :: Board.PieceType -> UI.UIPieceType
toUIPieceType Board.Man = UI.UIMan
toUIPieceType Board.King = UI.UIKing

-- | Convert UI piece type to game piece type
fromUIPieceType :: UI.UIPieceType -> Board.PieceType
fromUIPieceType UI.UIMan = Board.Man
fromUIPieceType UI.UIKing = Board.King
