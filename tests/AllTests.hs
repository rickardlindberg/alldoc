import Android
import Asserts
import DocPiece
import Fixtures()
import Test.Hspec.HUnit()
import Test.Hspec.Monadic
import Test.Hspec.QuickCheck
import Test.HUnit

main = hspecX $ do

    describe "Android SDK extractor:" $

        it "finds method names" $ do
            soup <- soupFromFile "sample-docs/android-Matrix.hml"
            extractMethodNames soup `assertContains` "setRotate"

    describe "doc pieces:" $ do

        it "can prefix with namespace" $ do
            let l = [DocPiece "Foo" "", DocPiece "Bar" ""]
            prefixWithNamespace "foo.bar" l @?= [Namespace "foo" "" [Namespace "bar" "" l]]

        describe "merging:" $ do

            it "can be merged" $ do
                let p1 = (DocPiece "" "")
                let p2 = (DocPiece "" "")
                merge [p1] [p2] @?= [p1, p2]

            prop "keeps number of leaf pieces" $ \(p1, p2) ->
                let numLeaves :: [DocPiece] -> Int
                    numLeaves xs = sum (map countLeaves xs)
                    countLeaves :: DocPiece -> Int
                    countLeaves (DocPiece _ _) = 1
                    countLeaves (Namespace _ _ s) = sum (map countLeaves s)
                in  numLeaves (merge p1 p2) == numLeaves p1 + numLeaves p2
