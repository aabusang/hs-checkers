-- |
-- Module      : UI.Controller
-- Description : UI state management and input handling
module UI.Controller 
    ( -- * Input Handling
      handleEvent
    , handleMouseMove
    ) where

import Graphics.Gloss.Interface.Pure.Game (Event)
import UI.Types
import UI.Input (handleInput)

-- | Handle all game events
handleEvent :: Event -> UIState -> UIState
handleEvent = handleInput

-- | Handle mouse movement
handleMouseMove :: UIState -> UIPosition -> UIState
handleMouseMove state pos =
    state { hoverPosition = Just pos }
