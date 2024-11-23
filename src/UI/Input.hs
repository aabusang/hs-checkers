-- |
-- Module      : UI.Input
-- Description : Input handling for the Checkers game
-- 
-- This module handles all user input, including mouse clicks and
-- converting screen coordinates to board positions.

module UI.Input
    ( -- * Input Handling
      handleInput
    , updateGame
    , runGame
      -- * Coordinate Conversion
    , screenToBoard
    ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Board.Types (Player(..), Piece(..), Position)
import Game.State (GameState(..), GameMode(..), initialGameState)
import Rules.Movement (isValidMove)
import Rules.Capture (makeMove)
import Rules.GameEnd (isGameOver)
import UI.Types (UIState(..), initialUIState)
import UI.Rendering (windowWidth, windowHeight, squareSize, drawGameState)
import AI.AIPlayer (makeAIMove)

-- | Converts screen coordinates to board coordinates
screenToBoard :: (Float, Float) -> Maybe Position
screenToBoard (x, y) =
    let offset = fromIntegral (windowWidth `div` 2) - squareSize * 4
        i = floor ((x + offset) / squareSize)
        j = floor ((y + offset) / squareSize)
    in if i >= 0 && i < 8 && j >= 0 && j < 8
       then Just (i, j)
       else Nothing

-- | Handles mouse input
handleInput :: Event -> UIState -> UIState
handleInput (EventKey (MouseButton LeftButton) Down _ (x, y)) (UIState gs mode) =
  let pos = screenToBoard (x, y)
  in if isGameOver gs
     then UIState initialGameState mode  -- Start a new game when game is over
     else case pos of
          Nothing -> UIState gs mode
          Just pos -> case selectedPiece gs of
            Nothing -> case board gs !! snd pos !! fst pos of
              Just (Piece p _) | p == currentPlayer gs ->
                UIState (gs { selectedPiece = Just pos }) mode
              _ -> UIState gs mode
            Just from ->
              if isValidMove gs from pos
              then let playerGs = makeMove gs from pos
                   in if isGameOver playerGs
                      then UIState playerGs mode  -- Game over after player's move
                      else case mode of
                           SinglePlayer _ -> UIState playerGs mode  -- AI will move in update
                           TwoPlayer -> UIState playerGs mode
              else UIState (gs { selectedPiece = Nothing }) mode
handleInput _ state = state

-- | Updates the game state
updateGame :: Float -> UIState -> UIState
updateGame _ state@(UIState gs mode)
  | case mode of
      SinglePlayer _ -> currentPlayer gs == Black && not (isGameOver gs)
      TwoPlayer -> False =
      state  -- AI move will be made in event handling
  | otherwise = state

-- | Starts the game
runGame :: GameMode -> IO ()
runGame mode = play window background fps (initialUIState mode)
                        drawGameState handleInput updateGame
  where
    window = InWindow "Checkers" (windowWidth, windowHeight) (10, 10)
    background = white
    fps = 30
