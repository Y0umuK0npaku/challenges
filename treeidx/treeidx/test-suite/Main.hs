import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec (Spec, describe, it, parallel, shouldBe)
import TreeLabeling (countValidLabelings, factorial, totalNodes)
import SubTreeLabeling (subtreeCount, generateTree)

main :: IO ()
main = do
    test <- testSpec "subtreeCount" spec
    defaultMain test

spec :: Spec
spec = parallel $ do
    describe "subtreeCount" $ do
        it "counts valid labelings for k=2, d=2" $ do
            let k = 2
            let d = 2
            let tree = generateTree k d
            let labelCount = subtreeCount tree

            labelCount `shouldBe` 80

        it "counts valid labelings for k=2, d=3" $ do
            let k = 2
            let d = 3
            let tree = generateTree k d
            let labelCount = subtreeCount tree

            labelCount `shouldBe` 21964800


        it "counts valid labelings for k=2, d=4" $ do
            let k = 2
            let d = 4
            let tree = generateTree k d
            let labelCount = subtreeCount tree

            labelCount `shouldBe` 74836825861835980800000

        it "counts valid labelings for k=3, d=2" $ do
            let k = 3
            let d = 2
            let tree = generateTree k d
            let labelCount = subtreeCount tree
            labelCount `shouldBe` 7484400

        it "counts valid labelings for k=3, d=3" $ do
            let k = 3
            let d = 3
            let tree = generateTree k d
            let labelCount = subtreeCount tree
            labelCount `shouldBe` 35417271278873496315860673177600000000


        it "counts valid labelings for k=10, d=1" $ do
            let k = 10
            let d = 1
            let tree = generateTree k d
            let labelCount = subtreeCount tree

            labelCount `shouldBe` 3628800
        
