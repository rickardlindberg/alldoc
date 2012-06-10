module Android where

import Data.Maybe
import Definitions
import TagSoupHelpers
import Text.HTML.TagSoup

scanTags :: [Tag String] -> [DefTree]
scanTags = fromMaybe [] . maybeClass

maybeClass :: [Tag String] -> Maybe [DefTree]
maybeClass tags = do
    ns     <- maybeClassNamespace tags
    defs   <- maybeClassDefinitions tags
    return $  prefixWithNamespace ns defs

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
