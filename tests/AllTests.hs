import qualified TestAndroidScanner
import qualified TestDefinitions
import qualified TestDirScanner
import Test.Hspec.HUnit()
import Test.Hspec.Monadic

main = hspecX $ do
    TestDefinitions.tests
    TestDirScanner.tests
    TestAndroidScanner.tests
