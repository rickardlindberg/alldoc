module Asserts where

import Control.Monad
import Definitions
import Test.HUnit

assertContains :: [DefTree] -> String -> Assertion
assertContains doc item =
    unless
        (docContains doc item)
        (assertFailure $ "Expected to find " ++ show item ++
                         " in " ++ show doc)
