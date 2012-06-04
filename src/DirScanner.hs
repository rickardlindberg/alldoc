module DirScanner where

import Definitions
import System.FilePath
import Text.HTML.TagSoup

scanHtmlDocs :: FilePath -> ([Tag String] -> [DefTree]) -> IO [DefTree]
scanHtmlDocs dir scanner = do
    files <- findHtmlFiles dir
    tags <- mapM soupFromFile files
    return $ foldr (merge . scanner) [] tags

findHtmlFiles :: FilePath -> IO [FilePath]
findHtmlFiles root = return [root </> "android/graphics/Matrix.html"]

soupFromFile :: FilePath -> IO [Tag String]
soupFromFile file = fmap parseTags (readFile file)
