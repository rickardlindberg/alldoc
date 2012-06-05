module Android where

import Data.Maybe
import Definitions
import TagSoupHelpers
import Text.HTML.TagSoup
import Text.StringLike

scanTags :: StringLike str => [Tag str] -> [DefTree]
scanTags = fromMaybe [] . maybeClass

maybeClass :: StringLike str => [Tag str] -> Maybe [DefTree]
maybeClass tags = do
    ns <- maybeClassNamespace tags
    defs <- maybeClassDefinitions tags
    return $ prefixWithNamespace ns defs

maybeClassNamespace :: StringLike str => [Tag str] -> Maybe String
maybeClassNamespace tags = Just tags
    >>= justSections (~== "<table class=jd-inheritance-table>")
    >>= maybeHead
    >>= justSections (~== "<td class=jd-inheritance-class-cell")
    >>= maybeLast
    >>= maybeFirstText

maybeClassDefinitions :: StringLike str => [Tag str] -> Maybe [DefTree]
maybeClassDefinitions tags = Just tags
    >>= justSections (~== "<div id=doc-content>")
    >>= maybeHead
    >>= justSections (~== "<span class=sympad>")
    >>= Just . map ((`Definition` "") . toString . ((\(TagText x) -> x) . head) . head . sections isTagText)
