module TestDirScanner (tests) where

import Definitions
import DirScanner
import Fixtures
import System.FilePath
import Test.Hspec.HUnit()
import Test.Hspec.Monadic
import Test.HUnit

tests = describe "dir scanner:" $ do

    it "finds html files recursively" $ withTmpDir $ \tmpDir -> do
        createEmptyFile $ tmpDir </> "a.html"
        createEmptyFile $ tmpDir </> "a.css"
        createEmptyFile $ tmpDir </> "sub" </> "b.html"
        createEmptyFile $ tmpDir </> "sub" </> "b.css"
        folded <- findHtmlFiles tmpDir
        folded @?= [ tmpDir </> "a.html"
                   , tmpDir </> "sub" </> "b.html"
                   ]

    it "scans html files in directory" $ do
        defTree <- scanHtmlDocs "sample-docs/android" (constScanner (Definition "" ""))
        defTree @?= [Definition "" "", Definition "" ""]

constScanner :: DefTree -> FileScanner
constScanner d _ _ = [d]
