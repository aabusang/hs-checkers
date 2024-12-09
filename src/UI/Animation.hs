module UI.Animation
    ( updateAnimations
    , AnimationState(..)
    ) where

import UI.Types

data AnimationState = 
    Idle
    | Capturing Float UIPosition
    deriving (Show, Eq)

updateAnimations :: Float -> UIState -> UIState
updateAnimations dt state =
    case lastCapture state of
        Nothing -> state
        Just _ -> 
            let newAnimation = captureAnimation state + dt
            in if newAnimation >= 1.0
               then state { lastCapture = Nothing, captureAnimation = 0.0 }
               else state { captureAnimation = newAnimation }