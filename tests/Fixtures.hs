module Fixtures where

import Control.Exception
import Control.Monad
import Definitions
import System.Directory
import System.FilePath
import System.IO
import Test.QuickCheck

instance Arbitrary DefTree where
    arbitrary = resize 10 $ sized tree
        where
            tree 0     = leaf
            tree n     = oneof [ leaf , node (n `div` 2) ]
            leaf       = liftM2 Definition (return "") (return "")
            node n     = liftM3 Namespace (nsName n) (return "") (subNodes n)
            nsName n   = return $ "ns" ++ show n
            subNodes n = do
                x <- listOf (tree n)
                return $ map prefix (zip [1..length x] x)
            prefix (n, Namespace a b c) = Namespace (show n ++ a) b c
            prefix (_, x)               = x
    shrink (Definition _ _)   = []
    shrink (Namespace _ _ []) = []
    shrink (Namespace a b _)  = [Namespace a b []]

withTmpDir :: (FilePath -> IO a) -> IO a
withTmpDir = bracket setUp tearDown
    where
        tmpDir   = "/tmp/alldoc-test"
        setUp    = createDirectory tmpDir >> return tmpDir
        tearDown = removeDirectoryRecursive

createEmptyFile :: FilePath -> IO FilePath
createEmptyFile path =
    createDirectoryIfMissing True (takeDirectory path) >>
    openFile path WriteMode >>= hClose >> return path
