-- |
-- Module      : UI.AIIntegration
-- Description : Integration of AI with the UI
-- 
-- This module handles the integration of AI player with the UI.

module UI.AIIntegration
    ( -- * AI Integration
      processAITurn
    ) where

import Graphics.Gloss.Data.Picture (Picture)
import UI.Types
import UI.Conversion (fromUIState, toUIState, toUIPosition)
import Game.State (selectedPiecePos)
import Game.Mode (GameMode(..))
import AI.Types (Difficulty(..))
import AI.Player (makeAIMove)

-- | Process AI turn if it's AI's turn
processAITurn :: Float -> UIState -> IO UIState
processAITurn _ uiState = 
    case gameMode uiState of
        SinglePlayer -> 
            let gameState' = fromUIState uiState
                currentPlayer = uiCurrentPlayer (UI.Types.gameState uiState)
            in if currentPlayer == UIWhite  -- Assuming AI plays as White
               then do
                   -- Make AI move
                   maybeNewGameState <- makeAIMove Medium gameState'
                   case maybeNewGameState of
                       Just newGameState -> 
                           -- Update UI state with AI's move
                           return $ uiState 
                               { gameState = gameState $ toUIState newGameState
                               , selectedPosition = case selectedPiecePos newGameState of
                                                     Just pos -> Just $ toUIPosition pos
                                                     Nothing -> Nothing
                               }
                       Nothing -> 
                           -- No valid moves for AI
                           return uiState
               else
                   -- Human player's turn
                   return uiState
        TwoPlayer ->
            -- Two human players, no AI needed
            return uiState
