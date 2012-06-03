import Test.Hspec.HUnit()
import Test.Hspec.Monadic
import Test.HUnit

main = hspecX $ do

    describe "the program" $ do

        it "does not exist yet" $
            1 @?= 2
