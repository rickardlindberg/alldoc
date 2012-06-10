module TestAndroidDocs (tests) where

import Android
import Asserts
import Definitions
import DirScanner
import Fixtures()
import Test.Hspec.HUnit()
import Test.Hspec.Monadic
import Test.HUnit

tests = describe "Android SDK extractor:" $ do

    it "finds method names" $ do
        soup <- soupFromFile "sample-docs/android/Matrix.html"
        scanTags soup `assertContains` "android.graphics.Matrix.setRotate"

    it "finds method urls" $ do
        soup <- soupFromFile "sample-docs/android/Matrix.html"
        let Just def = find "android.graphics.Matrix.setRotate" (scanTags soup)
        url def @?= "../../../reference/android/graphics/Matrix.html#setRotate(float)"
