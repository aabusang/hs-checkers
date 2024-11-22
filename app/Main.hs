module Main where

import UI
import System.IO
import Types (GameMode(..), Difficulty(..))

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Welcome to Checkers!"
  putStrLn "Select game mode:"
  putStrLn "1. Single Player (Easy)"
  putStrLn "2. Single Player (Medium)"
  putStrLn "3. Single Player (Hard)"
  putStrLn "4. Two Player"
  choice <- getLine
  let gameMode = case choice of
        "1" -> SinglePlayer Easy
        "2" -> SinglePlayer Medium
        "3" -> SinglePlayer Hard
        "4" -> TwoPlayer
        _   -> SinglePlayer Medium
  playCheckers gameMode