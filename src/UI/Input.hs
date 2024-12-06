module UI.Input
    ( handleInput
    , runGame
    ) where

import Graphics.Gloss.Interface.Pure.Game
import Board.Types (Piece(..), Board, Player(..), pieceOwner, PieceType(..))
import Types.Common (Position)
import Types.Game (GameState(..))  -- This gives us board and currentPlayer fields
import Game.Mode (GameMode(..))
import Game.State (makeMove, startNewGame, selectPiece)  -- Only import what's actually exported by Game.State
import UI.Types (UIState(..), initialUIState)
import UI.Shared (screenToBoardPosition, defaultConfig)
import UI.Rendering (drawGameState, windowWidth, windowHeight)
import Debug.Trace (trace)

-- | Convert screen position to board position
convertMousePosition :: Point -> Maybe Position
convertMousePosition mousePos = 
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
getPieceAt boardState (x, y) = boardState !! y !! x

-- | Format board for debug output
formatBoardState :: Board -> String
formatBoardState boardState = 
    unlines [ concat [ case piece of
                        Nothing -> "_"
                        Just (Piece Black _) -> "b"
                        Just (Piece White _) -> "w"
                    | piece <- row ]
            | row <- boardState ]

-- | Format piece for debug output
formatPiece :: Piece -> String
formatPiece (Piece owner _) = case owner of
    Black -> "b"
    White -> "w"

-- | Handle piece selection
handleSelection :: UIState -> Position -> UIState
handleSelection uiState pos =
    let gs = gameState uiState
    in trace ("Board state:\n" ++ formatBoardState (board gs)) $
       let currentBoard = board gs
           piece = getPieceAt currentBoard pos
       in case piece of
           Just p -> 
               if pieceOwner p == currentPlayer gs
               then trace ("Selected: " ++ formatPiece p) $ 
                    trySelectPiece uiState gs pos
               else trace ("Wrong player's piece (current: " ++ 
                          formatPiece (Piece (currentPlayer gs) Man) ++ ")") uiState
           Nothing -> trace "No piece found" uiState

-- | Try to select a piece based on game rules
trySelectPiece :: UIState -> GameState -> Position -> UIState
trySelectPiece uiState gs pos =
    trace ("Trying to select piece at: " ++ show pos) $
    case selectPiece gs pos of
        Just newGameState -> 
            uiState { gameState = newGameState
                   , selectedPosition = Just pos }
        Nothing -> 
            trace "Selection failed" uiState

-- | Handle piece movement
handleMovement :: UIState -> Position -> UIState
handleMovement uiState targetPos =
    trace ("HandleMovement called with targetPos: " ++ show targetPos) $
    case selectedPosition uiState of
        Nothing -> trace "No position selected" uiState
        Just fromPos -> trace ("Moving from: " ++ show fromPos) $ 
                       tryMove uiState fromPos targetPos

-- | Try to move a piece
tryMove :: UIState -> Position -> Position -> UIState
tryMove uiState fromPos toPos =
    let gs = gameState uiState
    in trace ("Attempting move from " ++ show fromPos ++ " to " ++ show toPos) $
       makeValidMove uiState gs fromPos toPos

-- | Execute a valid move
makeValidMove :: UIState -> GameState -> Position -> Position -> UIState
makeValidMove uiState gs fromPos toPos =
    case makeMove gs fromPos toPos of
        Just newGameState -> 
            uiState { gameState = newGameState
                   , selectedPosition = Nothing 
                   }
        Nothing -> 
            trace "Move failed" $ clearSelection uiState

-- | Clear the current selection
clearSelection :: UIState -> UIState
clearSelection uiState = uiState { selectedPosition = Nothing }

-- | Update game state (currently a no-op)
updateGame :: Float -> UIState -> UIState
updateGame _ state = state

-- | Render game state
renderGame :: UIState -> Picture
renderGame = drawGameState

-- | Handle input events (note the argument order)
handleInputEvent :: Event -> UIState -> UIState
handleInputEvent event state = handleInput state event

debugPrintBoard :: Board -> IO ()
debugPrintBoard = putStr . formatBoardState

-- | Run the game
runGame :: GameMode -> IO ()
runGame _mode = do
    let gs = startNewGame
        initialState = initialUIState gs
    debugPrintBoard (board $ gameState initialState)
    play window backgroundColor fps initialState renderGame handleInputEvent updateGame
    where
        window = InWindow "HaskellCheckers" 
                         (windowWidth, windowHeight) 
                         (10, 10)
        fps = 30
        backgroundColor = white