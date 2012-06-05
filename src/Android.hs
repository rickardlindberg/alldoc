module Android where

import Data.Maybe
import Definitions
import TagSoupHelpers
import Text.HTML.TagSoup
import Text.StringLike

parseClass :: StringLike str => [Tag str] -> [DefTree]
parseClass = fromJust . parseClassInner

parseClassInner :: StringLike str => [Tag str] -> Maybe [DefTree]
parseClassInner doc = do
    ns <- maybeClassNamespace doc
    rest <- maybeClass doc
    return $ prefixWithNamespace ns rest

maybeClass :: StringLike str => [Tag str] -> Maybe [DefTree]
maybeClass doc = do
     let a1 = sections (~== "<div id=doc-content>") doc
     a2 <- maybeHead a1
     let a3 = sections (~== "<span class=sympad>") a2
     let a4 = map ((`Definition` "") . toString . ((\(TagText x) -> x) . head) . head . sections isTagText) a3
     return a4

maybeClassNamespace :: StringLike str => [Tag str] -> Maybe String
maybeClassNamespace tags = do
    let a1 = sections (~== "<table class=jd-inheritance-table>") tags
    a2 <- maybeHead a1
    let a3 = sections (~== "<td class=jd-inheritance-class-cell") a2
    a4 <- maybeLast a3
    let a5 = firstText a4
    return a5
