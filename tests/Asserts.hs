module Asserts where

import Control.Monad
import Test.HUnit

assertContains :: (Eq a, Show a) => [a] -> a -> Assertion
assertContains list item =
    unless
        (item `elem` list)
        (assertFailure $ "Expected to find " ++ show item ++
                         " in " ++ show list)
