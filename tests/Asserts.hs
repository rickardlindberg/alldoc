module Asserts where

import Control.Monad
import DocPiece
import Test.HUnit

assertContains :: [DocPiece] -> String -> Assertion
assertContains doc item =
    unless
        (docContains doc item)
        (assertFailure $ "Expected to find " ++ show item ++
                         " in " ++ show doc)
