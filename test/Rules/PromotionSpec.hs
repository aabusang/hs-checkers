module Rules.PromotionSpec (spec) where

import Test.Hspec
import Board.Types
import Board.Construction (emptyBoard)
import Rules.Movement (updateBoard)
import Rules.Promotion

spec :: Spec
spec = do
    describe "shouldPromoteToKing" $ do
        it "promotes black pieces at row 7" $ do
            shouldPromoteToKing Black (0, 7) `shouldBe` True
            shouldPromoteToKing Black (7, 7) `shouldBe` True

        it "promotes white pieces at row 0" $ do
            shouldPromoteToKing White (0, 0) `shouldBe` True
            shouldPromoteToKing White (7, 0) `shouldBe` True

        it "doesn't promote pieces at other rows" $ do
            shouldPromoteToKing Black (0, 6) `shouldBe` False
            shouldPromoteToKing White (0, 1) `shouldBe` False
            shouldPromoteToKing Black (0, 0) `shouldBe` False
            shouldPromoteToKing White (0, 7) `shouldBe` False

    describe "promoteKings" $ do
        it "promotes black men to kings at row 7" $ do
            let board = updateBoard emptyBoard (3, 7) (Just $ Piece Black Man)
                promotedBoard = promoteKings board
            case promotedBoard !! 7 !! 3 of
                Just (Piece Black King) -> True `shouldBe` True
                _ -> expectationFailure "Black piece was not promoted to king"

        it "promotes white men to kings at row 0" $ do
            let board = updateBoard emptyBoard (3, 0) (Just $ Piece White Man)
                promotedBoard = promoteKings board
            case promotedBoard !! 0 !! 3 of
                Just (Piece White King) -> True `shouldBe` True
                _ -> expectationFailure "White piece was not promoted to king"

        it "doesn't promote pieces at non-promotion rows" $ do
            let board = updateBoard emptyBoard (3, 3) (Just $ Piece Black Man)
                promotedBoard = promoteKings board
            promotedBoard `shouldBe` board

        it "doesn't affect existing kings" $ do
            let board = updateBoard emptyBoard (3, 7) (Just $ Piece Black King)
                promotedBoard = promoteKings board
            promotedBoard `shouldBe` board

        it "handles multiple promotions simultaneously" $ do
            let board = foldr (uncurry updateBoard) emptyBoard
                    [ ((1, 7), Just $ Piece Black Man)
                    , ((3, 7), Just $ Piece Black Man)
                    , ((5, 0), Just $ Piece White Man)
                    ]
                promotedBoard = promoteKings board
            case (promotedBoard !! 7 !! 1, promotedBoard !! 7 !! 3, promotedBoard !! 0 !! 5) of
                (Just (Piece Black King), Just (Piece Black King), Just (Piece White King)) ->
                    True `shouldBe` True
                _ -> expectationFailure "Multiple pieces were not promoted correctly"
