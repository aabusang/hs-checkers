-- |
-- Module      : Board.Construction
-- Description : Board construction and initialization
-- 
-- This module handles the creation and initialization of checker boards,
-- providing functions to set up standard game positions and custom board layouts.

module Board.Construction 
    ( initialBoard
    , emptyBoard
    , standardGameSetup
    ) where

import Board.Types (Board, Piece(..), Player(..), PieceType(..))

-- | Creates an empty 8x8 board
emptyBoard :: Board
emptyBoard = replicate 8 (replicate 8 Nothing)

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

-- | Creates a standard game setup with initial board and Black as starting player
standardGameSetup :: (Board, Player)
standardGameSetup = (initialBoard, Black)
