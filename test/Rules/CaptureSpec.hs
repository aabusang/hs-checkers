module Rules.CaptureSpec (spec) where

import Test.Hspec
import Board.Types
import Board.Construction (emptyBoard)
import Game.State (GameState(..))
import Rules.Movement (updateBoard)
import Rules.Capture

spec :: Spec
spec = do
    describe "canJumpAgain" $ do
        it "detects when a piece can make another jump" $ do
            let board = foldr (uncurry updateBoard) emptyBoard
                    [ ((2, 2), Just $ Piece Black Man)
                    , ((3, 3), Just $ Piece White Man)
                    , ((5, 5), Just $ Piece White Man)
                    ]
            canJumpAgain board (2, 2) Black `shouldBe` True

        it "returns False when no more jumps are possible" $ do
            let board = foldr (uncurry updateBoard) emptyBoard
                    [ ((2, 2), Just $ Piece Black Man)
                    , ((5, 5), Just $ Piece White Man)
                    ]
            canJumpAgain board (2, 2) Black `shouldBe` False

        it "handles kings correctly" $ do
            let board = foldr (uncurry updateBoard) emptyBoard
                    [ ((2, 2), Just $ Piece Black King)
                    , ((3, 3), Just $ Piece White Man)
                    , ((3, 1), Just $ Piece White Man)
                    ]
            canJumpAgain board (2, 2) Black `shouldBe` True

    describe "makeMove" $ do
        let setupBoard = foldr (uncurry updateBoard) emptyBoard
                [ ((2, 2), Just $ Piece Black Man)
                , ((3, 3), Just $ Piece White Man)
                , ((5, 5), Just $ Piece White Man)
                ]
            initialState = GameState setupBoard Black Nothing

        it "updates the board after a regular move" $ do
            let newState = makeMove initialState (2, 2) (1, 3)
            board newState !! 2 !! 2 `shouldBe` Nothing
            board newState !! 3 !! 1 `shouldBe` Just (Piece Black Man)

        it "removes captured piece after a jump" $ do
            let newState = makeMove initialState (2, 2) (4, 4)
            board newState !! 2 !! 2 `shouldBe` Nothing
            board newState !! 3 !! 3 `shouldBe` Nothing  -- captured piece
            board newState !! 4 !! 4 `shouldBe` Just (Piece Black Man)

        it "keeps the same player's turn when more jumps are available" $ do
            let jumpState = makeMove initialState (2, 2) (4, 4)
            currentPlayer jumpState `shouldBe` Black
            selectedPiece jumpState `shouldBe` Just (4, 4)

        it "switches player's turn when no more jumps are available" $ do
            let moveState = makeMove initialState (2, 2) (1, 3)
            currentPlayer moveState `shouldBe` White
            selectedPiece moveState `shouldBe` Nothing

        it "handles king promotion after move" $ do
            let promotionBoard = updateBoard emptyBoard (6, 6) (Just $ Piece Black Man)
                promotionState = GameState promotionBoard Black Nothing
                newState = makeMove promotionState (6, 6) (7, 7)
            case board newState !! 7 !! 7 of
                Just (Piece Black King) -> True `shouldBe` True
                _ -> expectationFailure "Piece was not promoted to king after reaching last row"
