import qualified TestAndroidDocs
import qualified TestDefinitions
import Test.Hspec.HUnit()
import Test.Hspec.Monadic

main = hspecX $ do
    TestDefinitions.tests
    TestAndroidDocs.tests
