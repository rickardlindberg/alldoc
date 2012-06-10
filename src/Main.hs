module Main (main) where

import Definitions
import DirScanner
import qualified Scanner.Android as Android

main :: IO ()
main = do
    let androidApiDocDir = "/home/rick/downloads/android-sdk-linux/docs/reference/android/graphics/drawable/"
    defs <- scanHtmlDocs androidApiDocDir Android.scanFile
    mapM_ putStrLn (namespaces defs)
