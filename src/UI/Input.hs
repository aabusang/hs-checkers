module UI.Input
    ( runGame
    ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace (trace)


import UI.Types (UIState(..), initialUIState)
import Game.State (GameState(..))
import Board.Types (Position, Piece(..), Board, Player(..))
import Game.Mode (GameMode(..))
import Rules.Movement (getValidMoves)
import Rules.Capture (makeMove)
import qualified Game.State as GS
import qualified UI.Rendering as Rendering
import UI.Shared (screenToBoardPosition, defaultConfig)

-- | Convert screen position to board position
-- convertMousePosition :: Point -> Maybe Position
-- convertMousePosition mousePos = 
--     screenToBoardPosition defaultConfig mousePos

convertMousePosition :: Point -> Maybe Position
convertMousePosition mousePos@(x, y) = 
    trace ("Converting screen pos: " ++ show mousePos ++ 
           " to board pos: " ++ show converted) $
    converted
    where converted = screenToBoardPosition defaultConfig mousePos



-- | Process mouse click on the board
processMouseClick :: UIState -> Point -> UIState
processMouseClick uiState mousePos = 
    case convertMousePosition mousePos of
        Just boardPos -> handleMouseClick boardPos uiState
        Nothing -> uiState

-- | Main input handler
-- handleInput :: UIState -> Event -> UIState
-- handleInput uiState event =
--     case event of
--         EventKey (MouseButton LeftButton) Down _ mousePos ->
--             processMouseClick uiState mousePos
--         _ -> uiState

-- handleInput :: UIState -> Event -> UIState
-- handleInput uiState event =
--     trace ("Event received: " ++ show event) $
--     case event of
--         EventKey (MouseButton LeftButton) Down _ mousePos ->
--             trace ("Left click at: " ++ show mousePos) $
--             processMouseClick uiState mousePos
--         _ -> uiState

handleInput :: UIState -> Event -> UIState
handleInput uiState event =
    case event of
        EventKey (MouseButton LeftButton) Down _ mousePos ->
            trace ("Left click at: " ++ show mousePos) $
            processMouseClick uiState mousePos
        EventKey (MouseButton LeftButton) Up _ _ ->
            trace "Left click released" uiState
        -- Ignore all other events (including EventMotion)
        _ -> uiState


-- | Handle mouse click based on current game state
-- handleMouseClick :: Position -> UIState -> UIState
-- handleMouseClick pos uiState =
--     if isPositionSelected uiState
--     then handleMovement uiState pos
--     else handleSelection uiState pos

-- handleMouseClick :: Position -> UIState -> UIState
-- handleMouseClick pos uiState =
--     trace ("handleMouseClick called with pos: " ++ show pos) $
--     if isPositionSelected uiState
--     then handleMovement uiState pos
--     else trace ("Attempting selection at: " ++ show pos) $
--          handleSelection uiState pos

handleMouseClick :: Position -> UIState -> UIState
handleMouseClick pos uiState =
    trace ("handleMouseClick called with pos: " ++ show pos ++ 
           ", selected: " ++ show (selectedPosition uiState)) $
    if isPositionSelected uiState
    then trace "Handling movement" $ handleMovement uiState pos
    else trace "Handling selection" $ handleSelection uiState pos


-- | Check if a position is currently selected
isPositionSelected :: UIState -> Bool
isPositionSelected uiState = selectedPosition uiState /= Nothing

-- | Get piece at board position
getPieceAt :: Board -> Position -> Maybe Piece
getPieceAt board (x, y) = board !! y !! x

-- | Handle piece selection
-- handleSelection :: UIState -> Position -> UIState
-- handleSelection uiState pos =
--     let gs = gameState uiState
--         board = currentBoard gs
--         piece = getPieceAt board pos
--     in case piece of
--         Just _ -> trySelectPiece uiState gs pos
--         Nothing -> uiState

-- handleSelection :: UIState -> Position -> UIState
-- handleSelection uiState pos =
--     trace ("handleSelection called with pos: " ++ show pos) $
--     let gs = gameState uiState
--         board = currentBoard gs
--         piece = getPieceAt board pos
--     in case piece of
--         Just p -> trace ("Found piece: " ++ show p) $ 
--                  trySelectPiece uiState gs pos
--         Nothing -> trace "No piece found" uiState

-- handleSelection :: UIState -> Position -> UIState
-- handleSelection uiState pos =
--     trace ("handleSelection called with pos: " ++ show pos) $
--     let gs = gameState uiState
--         board = currentBoard gs
--         piece = getPieceAt board pos
--     in case piece of
--         Just p -> 
--             if piecePlayer p == currentPlayer gs
--             then trace ("Selected piece: " ++ show p) $ 
--                  trySelectPiece uiState gs pos
--             else trace "Wrong player's piece" uiState
--         Nothing -> trace "No piece found" uiState

handleSelection :: UIState -> Position -> UIState
handleSelection uiState pos =
    trace ("Board state at selection:\n" ++ 
           show (currentBoard (gameState uiState))) $
    let gs = gameState uiState
        board = currentBoard gs
        piece = getPieceAt board pos
    in case piece of
        Just p -> 
            if piecePlayer p == currentPlayer gs
            then trace ("Selected piece: " ++ show p) $ 
                 trySelectPiece uiState gs pos
            else trace ("Wrong player's piece (current player: " ++ 
                       show (currentPlayer gs) ++ ")") uiState
        Nothing -> trace "No piece found" uiState


-- | Try to select a piece based on game rules
-- trySelectPiece :: UIState -> GameState -> Position -> UIState
-- trySelectPiece uiState inputGameState pos =
--     case GS.selectPiece inputGameState pos of
--         Just newGameState -> 
--             uiState { gameState = newGameState, selectedPosition = Just pos }
--         Nothing -> uiState

trySelectPiece :: UIState -> GameState -> Position -> UIState
trySelectPiece uiState gs pos =
    trace ("Trying to select piece at: " ++ show pos) $
    uiState { selectedPosition = Just pos }

-- | Handle piece movement
-- handleMovement :: UIState -> Position -> UIState
-- handleMovement uiState targetPos =
--     case selectedPosition uiState of
--         Nothing -> uiState
--         Just fromPos -> tryMove uiState fromPos targetPos

-- Modify handleMovement to add tracing
handleMovement :: UIState -> Position -> UIState
handleMovement uiState targetPos =
    trace ("HandleMovement called with targetPos: " ++ show targetPos) $
    case selectedPosition uiState of
        Nothing -> trace "No position selected" uiState
        Just fromPos -> trace ("Moving from: " ++ show fromPos) $ 
                       tryMove uiState fromPos targetPos

-- | Try to move a piece
-- tryMove :: UIState -> Position -> Position -> UIState
-- tryMove uiState fromPos toPos =
--     let gs = gameState uiState
--     in if toPos `elem` getValidMoves gs fromPos
--        then makeValidMove uiState gs fromPos toPos
--        else clearSelection uiState

tryMove :: UIState -> Position -> Position -> UIState
tryMove uiState fromPos toPos =
    let gs = gameState uiState
        validMoves = getValidMoves gs fromPos
    in trace ("Valid moves: " ++ show validMoves) $
       if toPos `elem` validMoves
       then trace "Making valid move" $ makeValidMove uiState gs fromPos toPos
       else trace "Invalid move - clearing selection" $ clearSelection uiState



-- | Execute a valid move
makeValidMove :: UIState -> GameState -> Position -> Position -> UIState
makeValidMove uiState gs fromPos toPos =
    uiState { gameState = makeMove gs fromPos toPos
           , selectedPosition = Nothing 
           }

-- | Clear the current selection
clearSelection :: UIState -> UIState
clearSelection uiState = uiState { selectedPosition = Nothing }

-- | Update game state (currently a no-op)
updateGame :: Float -> UIState -> UIState
updateGame _ state = state

-- | Render game state
renderGame :: UIState -> Picture
renderGame = Rendering.drawGameState

-- | Handle input events (note the argument order)
handleInputEvent :: Event -> UIState -> UIState
handleInputEvent event state = handleInput state event


debugPrintBoard :: Board -> IO ()
debugPrintBoard board = 
    putStrLn $ unlines 
        [ concat [ case piece of
                    Nothing -> "."
                    Just (Piece Black _) -> "b"
                    Just (Piece White _) -> "w"
                | piece <- row ]
        | row <- board ]


-- | Run the game
-- runGame :: GameMode -> IO ()
-- runGame mode = 
--     play window backgroundColor fps initialState renderGame handleInputEvent updateGame
--     where
--         window = InWindow "HaskellCheckers" 
--                          (Rendering.windowWidth, Rendering.windowHeight) 
--                          (10, 10)
--         fps = 30
--         initialState = initialUIState mode
--         backgroundColor = white

-- Modify runGame to print initial board state
runGame :: GameMode -> IO ()
runGame mode = do
    let initialState = initialUIState mode
    debugPrintBoard (currentBoard $ gameState initialState)
    play window backgroundColor fps initialState renderGame handleInputEvent updateGame
    where
        window = InWindow "HaskellCheckers" 
                         (Rendering.windowWidth, Rendering.windowHeight) 
                         (10, 10)
        fps = 30
        backgroundColor = white