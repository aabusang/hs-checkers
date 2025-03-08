-- |
-- Module      : UI.Rendering
-- Description : Rendering functions for the Checkers game
-- 
-- This module handles all the graphical rendering using the Gloss library,
-- including drawing the board, pieces, and game state.
module UI.Rendering
    ( drawGameState
    ) where

-- External imports
import Graphics.Gloss
import UI.Types
import UI.Config (BoardConfig(..), boardConfig, PieceConfig(..), defaultPieceConfig, scaleFactor, squareSize, pieceScale, highlightScale, sidebarWidth, sidebarMargin, windowWidth, windowHeight)
import UI.Shared (boardToScreenPosition)
import UI.Board (squareColor)
import Game.Mode (GameMode(..))
import AI.Types (Difficulty(..))

-- | Get the color for a piece
pieceColor :: UIPiece -> Color
pieceColor piece = case uiPlayer piece of
    UIBlack -> makeColorI 40 40 40 255      -- Dark grey/black
    UIWhite -> makeColorI 245 245 245 255   -- Off-white

-- | Get a piece at a board position
getPieceAt :: UIGameState -> (Int, Int) -> Maybe UIPiece
getPieceAt state (row, col) = (uiBoard state) !! row !! col

-- | Draw the checkered board squares
drawSquares :: Picture
drawSquares =
    let config = boardConfig
        squareWidth = squareSize config * scaleFactor config
    in pictures
        [ translate screenPosX screenPosY $
          pictures [
              -- Base square
              color (squareColor (row, col)) $
              rectangleSolid squareWidth squareWidth,
              -- Grid lines for better visibility
              color (greyN 0.3) $
              rectangleWire squareWidth squareWidth
          ]
        | row <- [0..7]
        , col <- [0..7]
        , let (screenPosX, screenPosY) = boardToScreenPosition config (row, col)
        ]

-- | Draw a piece at a board position
drawPiece :: (Int, Int) -> UIPiece -> Picture
drawPiece pos piece =
    let config = boardConfig
        (screenX, screenY) = boardToScreenPosition config pos
        pieceConfig = defaultPieceConfig
        radius = squareSize config * scaleFactor config * pieceScale pieceConfig / 2
    in translate screenX screenY $
       color (pieceColor piece) $
       circleSolid radius

-- | Draw the game state
drawGameState :: UIState -> Picture
drawGameState state = 
    pictures $
        [ drawSquares
        , drawPieces (gameState state)
        , maybe blank drawHighlight (selectedPosition state)
        , drawSidebar state
        ]
  where
    drawPieces state = pictures
        [ maybe blank (drawPiece (row, col)) piece
        | row <- [0..7]
        , col <- [0..7]
        , let piece = getPieceAt state (row, col)
        ]
    
    drawHighlight position =
        let config = boardConfig
            (screenPosX, screenPosY) = boardToScreenPosition config position
            pieceConfig = defaultPieceConfig
            radius = squareSize config * scaleFactor config * 
                    pieceScale pieceConfig * highlightScale pieceConfig / 2
        in translate screenPosX screenPosY $
           color (makeColor 1 1 0 0.3) $  -- Semi-transparent yellow
           circleSolid radius
           
    -- Draw sidebar with game information and controls
    drawSidebar uiState =
        let 
            -- Convert window coordinates to sidebar coordinates
            sidebarX = -fromIntegral windowWidth / 2 + sidebarMargin
            sidebarTop = fromIntegral windowHeight / 2 - sidebarMargin
            
            -- Sidebar background
            sidebarBg = translate (sidebarX + sidebarWidth/2 - sidebarMargin) 0 $
                        color (makeColorI 240 240 240 255) $
                        rectangleSolid sidebarWidth (fromIntegral windowHeight)
            
            -- Title
            titleText = translate sidebarX (sidebarTop - 40) $
                        scale 0.25 0.25 $ color black $ text "CHECKERS"
            
            -- Current player info
            currentPlayer = case uiCurrentPlayer (gameState uiState) of
                UIBlack -> "Black"
                UIWhite -> "White"
            playerText = translate sidebarX (sidebarTop - 80) $
                         scale 0.2 0.2 $ color black $ text ("Current Player: " ++ currentPlayer)
            
            -- Game mode info
            modeText = translate sidebarX (sidebarTop - 110) $
                       scale 0.2 0.2 $ color black $ text (case gameMode uiState of
                           SinglePlayer -> "Mode: Single Player"
                           TwoPlayer -> "Mode: Two Players")
            
            -- Separator line
            separator1 = translate (sidebarX + sidebarWidth/2 - sidebarMargin) (sidebarTop - 130) $
                         color (greyN 0.7) $
                         rectangleSolid (sidebarWidth - 2*sidebarMargin) 1
            
            -- AI Difficulty section (only shown in SinglePlayer mode)
            difficultySection = case gameMode uiState of
                TwoPlayer -> blank
                SinglePlayer -> 
                    let 
                        -- Section title
                        sectionTitle = translate sidebarX (sidebarTop - 160) $
                                      scale 0.2 0.2 $ color black $ text "AI DIFFICULTY"
                        
                        -- Difficulty buttons
                        buttonY = sidebarTop - 200
                        buttonWidth = 50
                        buttonHeight = 30
                        buttonSpacing = 60
                        
                        -- Helper function for drawing a button
                        drawButton label x isSelected = 
                            pictures [
                                translate x buttonY $
                                color (if isSelected then makeColorI 100 149 237 255 else greyN 0.8) $
                                rectangleSolid buttonWidth buttonHeight,
                                translate x buttonY $
                                color black $
                                rectangleWire buttonWidth buttonHeight,
                                translate (x - 15) (buttonY - 5) $
                                scale 0.15 0.15 $ color black $ text label
                            ]
                        
                        -- Draw the three difficulty buttons
                        easyButton = drawButton "EASY" 
                                    (sidebarX + 30) 
                                    (aiDifficulty uiState == Easy)
                        mediumButton = drawButton "MED" 
                                      (sidebarX + 30 + buttonSpacing) 
                                      (aiDifficulty uiState == Medium)
                        hardButton = drawButton "HARD" 
                                    (sidebarX + 30 + 2*buttonSpacing) 
                                    (aiDifficulty uiState == Hard)
                        
                        -- Instructions
                        instructions = translate sidebarX (sidebarTop - 240) $
                                      scale 0.15 0.15 $ color (makeColorI 80 80 80 255) $ 
                                      text "Press keys 1-3 to change"
                    in 
                        pictures [sectionTitle, easyButton, mediumButton, hardButton, instructions]
            
            -- Instructions section
            separator2 = translate (sidebarX + sidebarWidth/2 - sidebarMargin) (sidebarTop - 270) $
                         color (greyN 0.7) $
                         rectangleSolid (sidebarWidth - 2*sidebarMargin) 1
            
            instructionsTitle = translate sidebarX (sidebarTop - 300) $
                               scale 0.2 0.2 $ color black $ text "INSTRUCTIONS"
            
            instructionsText = pictures
                [ translate sidebarX (sidebarTop - 330) $
                  scale 0.15 0.15 $ color black $ text "• Click to select a piece"
                , translate sidebarX (sidebarTop - 350) $
                  scale 0.15 0.15 $ color black $ text "• Click again to move"
                , translate sidebarX (sidebarTop - 370) $
                  scale 0.15 0.15 $ color black $ text "• Press ESC to deselect"
                ]
            
        in pictures [
            sidebarBg,
            titleText,
            playerText,
            modeText,
            separator1,
            difficultySection,
            separator2,
            instructionsTitle,
            instructionsText
        ]