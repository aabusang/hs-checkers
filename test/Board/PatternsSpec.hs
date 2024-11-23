module Board.PatternsSpec (spec) where

import Test.Hspec
import Board.Types
import Board.Construction (emptyBoard)
import Examples.CommonPatterns
import Examples.GameScenarios

spec :: Spec
spec = do
    describe "Common Board Patterns" $ do
        describe "doubleCornerPattern" $ do
            it "places pieces in correct corner positions" $ do
                let board = doubleCornerPattern Black
                board !! 0 !! 0 `shouldBe` Just (Piece Black Man)
                board !! 1 !! 1 `shouldBe` Just (Piece Black Man)
                board !! 0 !! 7 `shouldBe` Just (Piece Black Man)
                board !! 1 !! 6 `shouldBe` Just (Piece Black Man)

            it "leaves other squares empty" $ do
                let board = doubleCornerPattern White
                board !! 3 !! 3 `shouldBe` Nothing
                board !! 4 !! 4 `shouldBe` Nothing

        describe "triangleFormation" $ do
            it "creates a proper triangle shape" $ do
                let board = triangleFormation Black
                board !! 0 !! 3 `shouldBe` Just (Piece Black Man)
                board !! 1 !! 2 `shouldBe` Just (Piece Black Man)
                board !! 1 !! 4 `shouldBe` Just (Piece Black Man)
                board !! 2 !! 3 `shouldBe` Just (Piece Black Man)

    describe "Game Scenarios" $ do
        describe "multipleJumpScenario" $ do
            let GameState board player selected = multipleJumpScenario

            it "sets up pieces for multiple jumps" $ do
                board !! 2 !! 2 `shouldBe` Just (Piece Black Man)
                board !! 3 !! 3 `shouldBe` Just (Piece White Man)
                board !! 5 !! 5 `shouldBe` Just (Piece White Man)

            it "starts with Black's turn" $ do
                player `shouldBe` Black

        describe "kingPromotionScenario" $ do
            let GameState board player _ = kingPromotionScenario

            it "places a piece one move away from promotion" $ do
                board !! 6 !! 3 `shouldBe` Just (Piece Black Man)

            it "has opposing pieces to navigate around" $ do
                board !! 5 !! 5 `shouldBe` Just (Piece White Man)
                board !! 5 !! 1 `shouldBe` Just (Piece White Man)

        describe "endgameScenario" $ do
            let GameState board player _ = endgameScenario

            it "has correct piece setup" $ do
                board !! 0 !! 0 `shouldBe` Just (Piece Black King)
                board !! 7 !! 7 `shouldBe` Just (Piece White King)
                board !! 3 !! 3 `shouldBe` Just (Piece White Man)

            it "starts with White's turn" $ do
                player `shouldBe` White

        describe "stalemateSituation" $ do
            let GameState board player _ = stalemateSituation

            it "creates a proper stalemate position" $ do
                board !! 0 !! 0 `shouldBe` Just (Piece Black Man)
                board !! 1 !! 1 `shouldBe` Just (Piece White King)
                board !! 2 !! 0 `shouldBe` Just (Piece White King)
