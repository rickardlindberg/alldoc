module TagSoupHelpers where

import Data.Maybe
import Text.HTML.TagSoup
import Text.StringLike

maybeHead :: [a] -> Maybe a
maybeHead = listToMaybe

maybeLast :: [a] -> Maybe a
maybeLast = listToMaybe . reverse

firstText :: StringLike str => [Tag str] -> String
firstText = toString . ((\(TagText x) -> x) . head) . head . sections isTagText
