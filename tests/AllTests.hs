import Asserts
import Definitions
import Fixtures()
import qualified Data.Set as S
import qualified TestAndroidDocs
import Test.Hspec.HUnit()
import Test.Hspec.Monadic
import Test.Hspec.QuickCheck
import Test.HUnit

main = hspecX $ do

    TestAndroidDocs.tests

    describe "doc pieces:" $ do

        it "can prefix with namespace" $ do
            let l = [Definition "Foo" "", Definition "Bar" ""]
            prefixWithNamespace "foo.bar" l @?= [Namespace "foo" "" [Namespace "bar" "" l]]

        it "can check existence" $
            [Namespace "foo" "" [Namespace "bar" "" []]] `assertContains` "foo.bar"

        describe "merging:" $ do

            it "can be merged" $ do
                let p1 = Definition "" ""
                let p2 = Definition "" ""
                merge [p1] [p2] @?= [p1, p2]

            prop "keeps number of leaf pieces" $ \(p1, p2) ->
                let numLeaves :: [DefTree] -> Int
                    numLeaves xs = sum (map countLeaves xs)
                    countLeaves :: DefTree -> Int
                    countLeaves (Definition _ _) = 1
                    countLeaves (Namespace _ _ s) = sum (map countLeaves s)
                in  numLeaves (merge [p1] [p2]) == numLeaves [p1] + numLeaves [p2]

            prop "merges namespaces" $ \(p1, p2) ->
                let unique lst = length lst == S.size (S.fromList lst)
                in  unique (namespaces (merge [p1] [p2]))
