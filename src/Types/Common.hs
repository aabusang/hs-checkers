-- | Common types used across both UI and game logic
module Types.Common
    ( Position
    , ScreenPosition
    ) where

-- | Screen coordinates (Float for precise mouse position)
type ScreenPosition = (Float, Float)

-- | Board coordinates (Int for grid positions)
type Position = (Int, Int)
