module Asserts where

import Control.Monad
import Definitions
import Test.HUnit

assertContains :: [DefTree] -> String -> Assertion
assertContains tree definitionPath =
    unless
        (definitionPath `existsIn` tree)
        (assertFailure $ "Expected to find " ++ show definitionPath ++
                         " in " ++ show tree)
