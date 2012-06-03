module DocPiece where

import Data.List.Split

data DocPiece =
      Namespace
        { name      :: String
        , url       :: String
        , subPieces :: [DocPiece]
        }
    | DocPiece
        { name      :: String
        , url       :: String
        }
    deriving (Eq, Show)

merge :: [DocPiece] -> [DocPiece] -> [DocPiece]
merge a b = a ++ b

prefixWithNamespace :: String -> [DocPiece] -> [DocPiece]
prefixWithNamespace namespace items = [ns (splitNamespace namespace)]
    where
        ns [x]    = Namespace x "" items
        ns (x:xs) = Namespace x "" [ns xs]

splitNamespace :: String -> [String]
splitNamespace = splitOn "."
