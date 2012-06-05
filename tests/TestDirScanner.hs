module TestDirScanner (tests) where

import DirScanner
import Fixtures
import System.FilePath
import Test.Hspec.HUnit()
import Test.Hspec.Monadic
import Test.HUnit

tests = describe "DirScanner:" $

    it "finds html files recursively" $ withTmpDir $ \tmpDir -> do
        createEmptyFile $ tmpDir </> "a.html"
        createEmptyFile $ tmpDir </> "a.css"
        createEmptyFile $ tmpDir </> "sub" </> "b.html"
        createEmptyFile $ tmpDir </> "sub" </> "b.css"
        folded <- findHtmlFiles tmpDir
        folded @?= [ tmpDir </> "a.html"
                   , tmpDir </> "sub" </> "b.html"
                   ]
