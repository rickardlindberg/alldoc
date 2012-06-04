module TestAndroidDocs (tests) where

import Android
import Asserts
import Fixtures()
import Test.Hspec.HUnit()
import Test.Hspec.Monadic

tests = describe "Android SDK extractor:" $

    it "finds method names" $ do
        soup <- soupFromFile "sample-docs/android-Matrix.hml"
        parseClass soup `assertContains` "android.graphics.Matrix.setRotate"