module TestDefinitions (tests) where

import Asserts
import Definitions
import Fixtures()
import qualified Data.Set as S
import Test.Hspec.HUnit()
import Test.Hspec.Monadic
import Test.Hspec.QuickCheck
import Test.HUnit

tests = describe "Definitions:" $ do

    it "can be queried for existence" $ do
        let defs = [Namespace "foo" "" [Namespace "bar" "" []]]
        defs `assertContains` "foo.bar"

    it "can be prefixed with a namespace" $ do
        let defs = [Definition "Foo" "", Definition "Bar" ""]
        prefixWithNamespace "foo.bar" defs
            @?= [Namespace "foo" "" [Namespace "bar" "" defs]]

    it "can be merged" $ do
        let p1 = Definition "" ""
        let p2 = Definition "" ""
        merge [p1] [p2] @?= [p1, p2]

    describe "Merging:" $ do

        prop "keeps the number of leaves constant" $ \(p1, p2) ->
            let numLeaves :: [DefTree] -> Int
                numLeaves xs = sum (map countLeaves xs)
                countLeaves :: DefTree -> Int
                countLeaves (Definition _ _) = 1
                countLeaves (Namespace _ _ s) = sum (map countLeaves s)
            in  numLeaves (merge [p1] [p2]) == numLeaves [p1] + numLeaves [p2]

        prop "merges namespaces" $ \(p1, p2) ->
            let unique lst = length lst == S.size (S.fromList lst)
            in  unique (namespaces (merge [p1] [p2]))
