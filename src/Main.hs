module Main (main) where

import DirScanner
import qualified Android

main :: IO ()
main = do
    let androidApiDocDir = "/home/rick/downloads/android-sdk-linux/docs/reference/"
    defs <- scanHtmlDocs androidApiDocDir Android.parseClass
    print defs
