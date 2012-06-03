import Android
import Asserts
import Test.Hspec.HUnit()
import Test.Hspec.Monadic
import Test.HUnit()

main = hspecX $

    describe "Android SDK extractor:" $

        it "finds method names" $ do
            soup <- soupFromFile "sample-docs/android-Matrix.hml"
            extractMethodNames soup `assertContains` "setRotate"
