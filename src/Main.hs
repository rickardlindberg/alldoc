module Main (main) where

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
    deriving (Show)

main :: IO ()
main = putStrLn "not implemented yet"
