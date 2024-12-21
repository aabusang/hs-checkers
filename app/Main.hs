module Main where

import UI.Game (runGame)
import Game.Mode (GameMode(..))
import System.IO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "Welcome to Checkers!"
    putStrLn "Select game mode:"
    putStrLn "1. Single Player"
    putStrLn "2. Two Player"
    choice <- getLine
    let gameMode = case choice of
            "1" -> SinglePlayer
            _   -> TwoPlayer  -- Default to TwoPlayer mode
    runGame gameMode