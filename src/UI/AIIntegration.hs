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
import UI.Types (UIState(..), UIPlayer(..), UIGameState(..), gameState, gameMode, selectedPosition, aiThinkingState, AIThinkingState(..), aiDifficulty)
import UI.Conversion (fromUIState, toUIState, toUIPosition)
import Game.State (selectedPiecePos)
import Game.Mode (GameMode(..))
import AI.Types (Difficulty(..))
import AI.Player (makeAIMove)

-- | AI thinking time in seconds before making a move
aiThinkingTime :: Float
aiThinkingTime = 1.0  -- Adjust this value to control AI response time (in seconds)

-- | Process AI turn if it's AI's turn
processAITurn :: Float -> UIState -> IO UIState
processAITurn dt uiState = 
    case gameMode uiState of
        SinglePlayer -> 
            let gameState' = fromUIState uiState
                currentPlayer = uiCurrentPlayer (UI.Types.gameState uiState)
            in if currentPlayer == UIWhite  -- Assuming AI plays as White
               then case aiThinkingState uiState of
                    Idle -> 
                        -- Start AI thinking
                        return $ uiState { aiThinkingState = Thinking 0.0 }
                    
                    Thinking elapsedTime ->
                        let newElapsedTime = elapsedTime + dt
                        in if newElapsedTime >= aiThinkingTime
                           then
                               -- AI is done thinking, ready to move
                               return $ uiState { aiThinkingState = ReadyToMove }
                           else
                               -- AI is still thinking
                               return $ uiState { aiThinkingState = Thinking newElapsedTime }
                    
                    ReadyToMove -> do
                        -- Make AI move using the selected difficulty level
                        maybeNewGameState <- makeAIMove (aiDifficulty uiState) gameState'
                        case maybeNewGameState of
                            Just newGameState -> 
                                -- Update UI state with AI's move
                                return $ uiState 
                                    { gameState = gameState $ toUIState newGameState
                                    , selectedPosition = case selectedPiecePos newGameState of
                                                          Just pos -> Just $ toUIPosition pos
                                                          Nothing -> Nothing
                                    , aiThinkingState = Idle
                                    }
                            Nothing -> 
                                -- No valid moves for AI
                                return $ uiState { aiThinkingState = Idle }
               else
                   -- Human player's turn, reset AI thinking state
                   return $ uiState { aiThinkingState = Idle }
        TwoPlayer ->
            -- Two human players, no AI needed
            return uiState
