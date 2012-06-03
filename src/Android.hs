module Android where

import Text.HTML.TagSoup
import Text.StringLike

soupFromFile :: FilePath -> IO [Tag String]
soupFromFile file = fmap parseTags (readFile file)

extractMethodNames :: StringLike str => [Tag str] -> [str]
extractMethodNames =
       map (((\(TagText x) -> x) . head) . head . sections isTagText)
     . sections (~== "<span class=sympad>")
     . head
     . sections (~== "<div id=doc-content>")
