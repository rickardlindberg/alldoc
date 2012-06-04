module DocPiece where

import Data.List.Split

data DefTree =
      Namespace
        { name      :: String
        , url       :: String
        , subPieces :: [DefTree]
        }
    | Definition
        { name      :: String
        , url       :: String
        }
    deriving (Eq, Show)

docContains :: [DefTree] -> String -> Bool
docContains doc path = matchDoc (splitNamespace path) doc
    where
        matchDoc :: [String] -> [DefTree] -> Bool
        matchDoc []     doc = False
        matchDoc [x]    doc = length (namesMatching x doc) > 0
        matchDoc (x:xs) doc = matchDoc xs (expand (namesMatching x doc))
        expand :: [DefTree] -> [DefTree]
        expand []                   = []
        expand (Namespace _ _ x:xs) = x ++ expand xs
        expand (_:xs)               = expand xs
        namesMatching :: String -> [DefTree] -> [DefTree]
        namesMatching n = filter (hasName n)
        hasName :: String -> DefTree -> Bool
        hasName n thing = name thing == n

merge :: [DefTree] -> [DefTree] -> [DefTree]
merge []     b = b
merge (a:as) b = merge as (mergeOne a b)
    where
        mergeOne :: DefTree -> [DefTree] -> [DefTree]
        mergeOne a []     = [a]
        mergeOne a (b:bs)
            | sameNamespace a b = Namespace (name a) (url a) (merge (subPieces a) (subPieces b)) : bs
            | otherwise         = b : mergeOne a bs

sameNamespace :: DefTree -> DefTree -> Bool
sameNamespace (Namespace a1 a2 a3) (Namespace b1 b2 b3) = a1 == b1
sameNamespace _                    _                    = False

namespaces :: [DefTree] -> [String]
namespaces [] = []
namespaces (Namespace n _ ns:xs) = n : map (\x -> n ++ "." ++ x) (namespaces ns) ++ namespaces xs
namespaces (_:xs) = namespaces xs

prefixWithNamespace :: String -> [DefTree] -> [DefTree]
prefixWithNamespace namespace items = [ns (splitNamespace namespace)]
    where
        ns [x]    = Namespace x "" items
        ns (x:xs) = Namespace x "" [ns xs]

splitNamespace :: String -> [String]
splitNamespace = splitOn "."
