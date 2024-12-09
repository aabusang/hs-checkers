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
    , placePiece
    , removePiece
    ) where

import Board.Types (Board, Piece(..), Player(..), PieceType(..))
import Board.Validation (isValidBoardPosition)
import Types.Common (Position)

-- | Creates an empty 8x8 board
emptyBoard :: Board
emptyBoard = replicate 8 (replicate 8 Nothing)

-- | Creates the initial checker board setup with all pieces on dark squares (where x + y is even).
initialBoard :: Board
initialBoard = [
    -- Black pieces (top)
    [Just (Piece Black Man), Nothing, Just (Piece Black Man), Nothing, Just (Piece Black Man), Nothing, Just (Piece Black Man), Nothing],
    [Nothing, Just (Piece Black Man), Nothing, Just (Piece Black Man), Nothing, Just (Piece Black Man), Nothing, Just (Piece Black Man)],
    [Just (Piece Black Man), Nothing, Just (Piece Black Man), Nothing, Just (Piece Black Man), Nothing, Just (Piece Black Man), Nothing],
    -- Empty middle
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    -- White pieces (bottom)
    [Nothing, Just (Piece White Man), Nothing, Just (Piece White Man), Nothing, Just (Piece White Man), Nothing, Just (Piece White Man)],
    [Just (Piece White Man), Nothing, Just (Piece White Man), Nothing, Just (Piece White Man), Nothing, Just (Piece White Man), Nothing],
    [Nothing, Just (Piece White Man), Nothing, Just (Piece White Man), Nothing, Just (Piece White Man), Nothing, Just (Piece White Man)]
    ]

-- | Creates a standard game setup with initial board and Black as starting player
standardGameSetup :: (Board, Player)
standardGameSetup = (initialBoard, Black)

-- | Places a piece at the specified position on the board
-- Returns the original board if the position is invalid
placePiece :: Position -> Piece -> Board -> Board
placePiece (x, y) piece board
    | not (isValidBoardPosition (x, y)) = board
    | otherwise = take y board ++ 
                 [take x (board !! y) ++ [Just piece] ++ drop (x + 1) (board !! y)] ++ 
                 drop (y + 1) board

-- | Removes a piece from the specified position
-- Returns the original board if the position is invalid
removePiece :: Position -> Board -> Board
removePiece (x, y) board
    | not (isValidBoardPosition (x, y)) = board
    | otherwise = take y board ++
                 [take x (board !! y) ++ [Nothing] ++ drop (x + 1) (board !! y)] ++
                 drop (y + 1) board
