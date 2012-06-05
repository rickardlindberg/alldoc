import qualified TestAndroidDocs
import qualified TestDefinitions
import qualified TestDirScanner
import Test.Hspec.HUnit()
import Test.Hspec.Monadic

main = hspecX $ do
    TestDefinitions.tests
    TestDirScanner.tests
    TestAndroidDocs.tests
