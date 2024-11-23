module Rules.MovementSpec (spec) where

import Test.Hspec
import Board.Types
import Board.Construction (emptyBoard)
import Game.State (GameState(..))
import Rules.Movement

spec :: Spec
spec = do
    describe "isWithinBoard" $ do
        it "returns True for valid positions" $ do
            isWithinBoard (0, 0) `shouldBe` True
            isWithinBoard (7, 7) `shouldBe` True
            isWithinBoard (3, 4) `shouldBe` True

        it "returns False for positions outside the board" $ do
            isWithinBoard (-1, 0) `shouldBe` False
            isWithinBoard (8, 7) `shouldBe` False
            isWithinBoard (3, -1) `shouldBe` False
            isWithinBoard (3, 8) `shouldBe` False

    describe "isValidDirection" $ do
        let blackMan = Piece Black Man
            whiteMan = Piece White Man
            blackKing = Piece Black King
            whiteKing = Piece White King

        it "allows black men to move down" $ do
            isValidDirection blackMan (3, 3) (4, 4) `shouldBe` True
            isValidDirection blackMan (3, 3) (2, 4) `shouldBe` True

        it "prevents black men from moving up" $ do
            isValidDirection blackMan (3, 3) (4, 2) `shouldBe` False
            isValidDirection blackMan (3, 3) (2, 2) `shouldBe` False

        it "allows white men to move up" $ do
            isValidDirection whiteMan (3, 3) (4, 2) `shouldBe` True
            isValidDirection whiteMan (3, 3) (2, 2) `shouldBe` True

        it "prevents white men from moving down" $ do
            isValidDirection whiteMan (3, 3) (4, 4) `shouldBe` False
            isValidDirection whiteMan (3, 3) (2, 4) `shouldBe` False

        it "allows kings to move in any direction" $ do
            isValidDirection blackKing (3, 3) (4, 4) `shouldBe` True
            isValidDirection blackKing (3, 3) (2, 2) `shouldBe` True
            isValidDirection whiteKing (3, 3) (4, 4) `shouldBe` True
            isValidDirection whiteKing (3, 3) (2, 2) `shouldBe` True

    describe "isValidMove" $ do
        let emptyGameState = GameState emptyBoard Black Nothing
            singlePieceBoard = updateBoard emptyBoard (3, 3) (Just $ Piece Black Man)
            gameStateWithPiece = GameState singlePieceBoard Black Nothing

        it "prevents moving from an empty square" $ do
            isValidMove emptyGameState (0, 0) (1, 1) `shouldBe` False

        it "prevents moving to an occupied square" $ do
            let boardWithTwoPieces = updateBoard singlePieceBoard (4, 4) (Just $ Piece White Man)
            let gameState = GameState boardWithTwoPieces Black Nothing
            isValidMove gameState (3, 3) (4, 4) `shouldBe` False

        it "allows valid diagonal moves" $ do
            isValidMove gameStateWithPiece (3, 3) (4, 4) `shouldBe` True
            isValidMove gameStateWithPiece (3, 3) (2, 4) `shouldBe` True

        it "prevents non-diagonal moves" $ do
            isValidMove gameStateWithPiece (3, 3) (3, 4) `shouldBe` False
            isValidMove gameStateWithPiece (3, 3) (4, 3) `shouldBe` False

    describe "getValidMoves" $ do
        let singlePieceBoard = updateBoard emptyBoard (3, 3) (Just $ Piece Black Man)
            gameState = GameState singlePieceBoard Black Nothing

        it "returns empty list for empty square" $ do
            getValidMoves gameState (0, 0) `shouldBe` []

        it "returns valid moves for a piece" $ do
            let moves = getValidMoves gameState (3, 3)
            moves `shouldContain` [(2, 4)]
            moves `shouldContain` [(4, 4)]
            length moves `shouldBe` 2

        it "returns empty list for opponent's piece" $ do
            let boardWithWhitePiece = updateBoard emptyBoard (3, 3) (Just $ Piece White Man)
                gameStateWithWhite = GameState boardWithWhitePiece Black Nothing
            getValidMoves gameStateWithWhite (3, 3) `shouldBe` []
