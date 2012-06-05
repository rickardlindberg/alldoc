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
maybeClassNamespace tags = do
    let a1 = sections (~== "<table class=jd-inheritance-table>") tags
    a2 <- maybeHead a1
    let a3 = sections (~== "<td class=jd-inheritance-class-cell") a2
    a4 <- maybeLast a3
    maybeFirstText a4

maybeClassDefinitions :: StringLike str => [Tag str] -> Maybe [DefTree]
maybeClassDefinitions tags = do
     let a1 = sections (~== "<div id=doc-content>") tags
     a2 <- maybeHead a1
     let a3 = sections (~== "<span class=sympad>") a2
     let a4 = map ((`Definition` "") . toString . ((\(TagText x) -> x) . head) . head . sections isTagText) a3
     return a4
