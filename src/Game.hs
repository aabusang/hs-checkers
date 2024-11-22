{-|
Module      : Game
Description : Core game logic for the Checkers game
Copyright   : (c) Your Name, 2024
License     : Your License

This module handles the core game mechanics, including:
  * Setting up the initial game board
  * Managing game state
-}
module Game 
    ( -- * Game Setup
      initialBoard
    , initialGameState
    ) where

import Types (Player(..), PieceType(..), Piece(..), Position, Board, GameState(..))

-- | Creates the initial checker board setup.
-- Black pieces are at the top, White pieces at the bottom.
-- Each player starts with 12 pieces arranged on the dark squares
-- of the first three rows on their side.
initialBoard :: Board
initialBoard = [
    [Just (Piece Black Man), Nothing, Just (Piece Black Man), Nothing, Just (Piece Black Man), Nothing, Just (Piece Black Man), Nothing],
    [Nothing, Just (Piece Black Man), Nothing, Just (Piece Black Man), Nothing, Just (Piece Black Man), Nothing, Just (Piece Black Man)],
    [Just (Piece Black Man), Nothing, Just (Piece Black Man), Nothing, Just (Piece Black Man), Nothing, Just (Piece Black Man), Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Just (Piece White Man), Nothing, Just (Piece White Man), Nothing, Just (Piece White Man), Nothing, Just (Piece White Man)],
    [Just (Piece White Man), Nothing, Just (Piece White Man), Nothing, Just (Piece White Man), Nothing, Just (Piece White Man), Nothing],
    [Nothing, Just (Piece White Man), Nothing, Just (Piece White Man), Nothing, Just (Piece White Man), Nothing, Just (Piece White Man)]
    ]

-- | Creates the initial game state.
-- The game starts with White player's turn and no piece selected.
initialGameState :: GameState
initialGameState = GameState initialBoard White Nothing