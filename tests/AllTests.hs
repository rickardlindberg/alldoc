import Test.Hspec.HUnit()
import Test.Hspec.Monadic
import Test.HUnit

main = hspecX $

    describe "the program" $

        it "does not exist yet" $
            1 @?= 1
