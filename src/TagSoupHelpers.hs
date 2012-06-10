module TagSoupHelpers where

import Data.Maybe
import qualified Data.Map as M
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

maybeN :: Int -> [a] -> Maybe a
maybeN n list = if length list > n then Just (list !! n) else Nothing

maybeText :: Tag String -> Maybe String
maybeText (TagText text) = Just text
maybeText _              = Nothing

maybeAttribute :: String -> Tag String -> Maybe String
maybeAttribute key (TagOpen _ attrs) = M.lookup key (M.fromList attrs)
maybeAttribute _   _                 = Nothing

maybeHead :: [a] -> Maybe a
maybeHead = listToMaybe

maybeLast :: [a] -> Maybe a
maybeLast = listToMaybe . reverse
