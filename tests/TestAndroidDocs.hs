module TestAndroidDocs (tests) where

import Asserts
import Definitions
import DirScanner
import Fixtures()
import qualified Scanner.Android as Android
import Test.Hspec.HUnit()
import Test.Hspec.Monadic
import Test.HUnit

tests = describe "AndroidDocs:" $ do

    let scanFile f = fmap (Android.scanFile f) (soupFromFile f)

    it "finds method names" $ do
        defs <- scanFile "sample-docs/android/Matrix.html"
        defs `assertContains` "android.graphics.Matrix.setRotate"

    it "finds method urls" $ do
        defs <- scanFile "sample-docs/android/Matrix.html"
        let Just def = find "android.graphics.Matrix.setRotate" defs
        url def @?= "../../../reference/android/graphics/Matrix.html#setRotate(float)"
