module DocPiece where

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
