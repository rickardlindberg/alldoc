module DirScanner where

import Control.Monad
import Data.List
import Definitions
import qualified Data.ByteString.Char8 as B
import System.Directory
import System.FilePath
import Text.HTML.TagSoup

scanHtmlDocs :: FilePath -> (FilePath -> [Tag String] -> [DefTree]) -> IO [DefTree]
scanHtmlDocs dir scanner = do
    files  <- findHtmlFiles dir
    tags   <- mapM soupFromFile files
    return $  foldr foldTrees [] tags
    where
        foldTrees :: [Tag String] -> [DefTree] -> [DefTree]
        foldTrees x y = scanner "TODO: fill in the right thing here" x ++ y

findHtmlFiles :: FilePath -> IO [FilePath]
findHtmlFiles root = do
    htmlFiles    <- htmlFilesIn root
    subHtmlFiles <- directoriesIn root >>= mapM (\x -> findHtmlFiles (root </> x))
    return $ htmlFiles ++ concat subHtmlFiles

htmlFilesIn :: FilePath -> IO [FilePath]
htmlFilesIn dir = do
    files <- getDirectoryContents dir
    let htmlFiles = filter (".html" `isSuffixOf`) files
    return $ map (dir </>) htmlFiles

directoriesIn :: FilePath -> IO [FilePath]
directoriesIn dir = do
    files <- getDirectoryContents dir
    let regularFiles = filter (`notElem` [".", ".."]) files
    filterM (doesDirectoryExist . (dir </>)) regularFiles

soupFromFile :: FilePath -> IO [Tag String]
soupFromFile file = fmap parseTags (slurp file)

slurp :: FilePath -> IO String
slurp file = fmap B.unpack (B.readFile file)
