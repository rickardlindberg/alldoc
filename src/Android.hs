module Android where

import Definitions
import Text.HTML.TagSoup
import Text.StringLike

parseClass :: StringLike str => [Tag str] -> [DefTree]
parseClass doc =
       (prefixWithNamespace (parseClassNamespaceName doc)
     . map ((`Definition` "") . toString . ((\(TagText x) -> x) . head) . head . sections isTagText)
     . sections (~== "<span class=sympad>")
     . head
     . sections (~== "<div id=doc-content>"))
       doc

parseClassNamespaceName :: StringLike str => [Tag str] -> String
parseClassNamespaceName =
       firstText
     . last
     . sections (~== "<td class=jd-inheritance-class-cell")
     . head
     . sections (~== "<table class=jd-inheritance-table>")

firstText :: StringLike str => [Tag str] -> String
firstText = toString . ((\(TagText x) -> x) . head) . head . sections isTagText
