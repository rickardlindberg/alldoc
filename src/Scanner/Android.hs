module Scanner.Android where

import Data.Maybe
import Definitions
import System.FilePath
import TagSoupHelpers
import Text.HTML.TagSoup

scanFile :: FilePath -> [Tag String] -> [DefTree]
scanFile file = fromMaybe [] . maybeClass file

maybeClass :: FilePath -> [Tag String] -> Maybe [DefTree]
maybeClass file tags = do
    ns     <- maybeClassNamespace tags
    defs   <- maybeClassDefinitions tags
    return $  prefixWithNamespace ns (map (makeUrlAbsolute file) defs)

makeUrlAbsolute :: FilePath -> DefTree -> DefTree
makeUrlAbsolute file (Definition name url) = Definition name absUrl
    where
        absUrl = "file://" ++ (takeDirectory file </> url)
makeUrlAbsolute file tree                  = tree

maybeClassNamespace :: [Tag String] -> Maybe String
maybeClassNamespace tags = Just tags
    >>= justSections (~== "<table class=jd-inheritance-table>")
    >>= maybeHead
    >>= justSections (~== "<td class=jd-inheritance-class-cell")
    >>= maybeLast
    >>= maybeFirstText

maybeClassDefinitions :: [Tag String] -> Maybe [DefTree]
maybeClassDefinitions tags = Just tags
    >>= justSections (~== "<div id=doc-content>")
    >>= maybeHead
    >>= justSections (~== "<span class=sympad>")
    >>= Just . mapMaybe maybeMethodDef

maybeMethodDef :: [Tag String] -> Maybe DefTree
maybeMethodDef sympadTags = do
    url    <- maybeN 1 sympadTags >>= maybeAttribute "href"
    name   <- maybeN 2 sympadTags >>= maybeText
    return $  Definition name url
