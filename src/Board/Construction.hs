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
    , isValidPosition
    , placePiece
    , removePiece
    ) where

import Board.Types (Board, Piece(..), Player(..), PieceType(..))

-- | Creates an empty 8x8 board
emptyBoard :: Board
emptyBoard = replicate 8 (replicate 8 Nothing)

-- | Creates the initial checker board setup with pieces on dark squares.
initialBoard :: Board
initialBoard = [
    -- White pieces (top)
    [Nothing, Just (Piece White Man), Nothing, Just (Piece White Man), Nothing, Just (Piece White Man), Nothing, Just (Piece White Man)],
    [Just (Piece White Man), Nothing, Just (Piece White Man), Nothing, Just (Piece White Man), Nothing, Just (Piece White Man), Nothing],
    [Nothing, Just (Piece White Man), Nothing, Just (Piece White Man), Nothing, Just (Piece White Man), Nothing, Just (Piece White Man)],
    -- Empty middle
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    -- Black pieces (bottom)
    [Just (Piece Black Man), Nothing, Just (Piece Black Man), Nothing, Just (Piece Black Man), Nothing, Just (Piece Black Man), Nothing],
    [Nothing, Just (Piece Black Man), Nothing, Just (Piece Black Man), Nothing, Just (Piece Black Man), Nothing, Just (Piece Black Man)],
    [Just (Piece Black Man), Nothing, Just (Piece Black Man), Nothing, Just (Piece Black Man), Nothing, Just (Piece Black Man), Nothing]
    ]

-- | Creates a standard game setup with initial board and Black as starting player
standardGameSetup :: (Board, Player)
standardGameSetup = (initialBoard, Black)

-- | Checks if a position is within the bounds of the board (8x8)
isValidPosition :: (Int, Int) -> Bool
isValidPosition (x, y) = x >= 0 && x < 8 && y >= 0 && y < 8

-- | Places a piece at the specified position on the board
-- Returns the original board if the position is invalid
placePiece :: (Int, Int) -> Piece -> Board -> Board
placePiece (x, y) piece board
    | not (isValidPosition (x, y)) = board
    | otherwise = take x board ++ 
                 [take y (board !! x) ++ [Just piece] ++ drop (y + 1) (board !! x)] ++ 
                 drop (x + 1) board

-- | Removes a piece from the specified position
-- Returns the original board if the position is invalid
removePiece :: (Int, Int) -> Board -> Board
removePiece (x, y) board
    | not (isValidPosition (x, y)) = board
    | otherwise = take x board ++ 
                 [take y (board !! x) ++ [Nothing] ++ drop (y + 1) (board !! x)] ++ 
                 drop (x + 1) board
