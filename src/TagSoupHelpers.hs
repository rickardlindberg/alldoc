module TagSoupHelpers where

import Data.Maybe
import Text.HTML.TagSoup
import Text.StringLike

justSections a = Just . sections a

maybeFirstText :: StringLike str => [Tag str] -> Maybe String
maybeFirstText tags = do
    let a1 = sections isTagText tags
    a2 <- maybeHead a1
    let a3 = ((\(TagText x) -> x) . head) a2
    let a4 = toString a3
    return a4

maybeHead :: [a] -> Maybe a
maybeHead = listToMaybe

maybeLast :: [a] -> Maybe a
maybeLast = listToMaybe . reverse
