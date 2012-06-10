module TestDefinitions (tests) where

import Asserts
import Definitions
import Fixtures()
import Test.Hspec.HUnit()
import Test.Hspec.Monadic
import Test.Hspec.QuickCheck
import Test.HUnit

tests = describe "definitions:" $ do

    it "can be queried for existence" $ do
        let defs = [Namespace "foo" "" [Namespace "bar" "" []]]
        defs `assertContains` "foo.bar"

    it "can be prefixed with a namespace" $ do
        let defs = [Definition "Foo" "", Definition "Bar" ""]
        prefixWithNamespace "foo.bar" defs
            @?= [Namespace "foo" "" [Namespace "bar" "" defs]]

    it "can be merged" $ do
        let def1 = Definition "foo" ""
        let def2 = Definition "bar" ""
        merge [def1] [def2] @?= [def2, def1]

    describe "merging:" $ do

        prop "keeps the number of leaves constant" $ \(def1, def2) ->
            numLeaves (merge [def1] [def2]) == numLeaves [def1] + numLeaves [def2]

        prop "merges namespaces" $ \(def1, def2) ->
            isUnique (namespaces (merge [def1] [def2]))
