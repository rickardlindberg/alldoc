module Asserts where

import Control.Monad
import Definitions
import qualified Data.Set as S
import Test.HUnit

assertContains :: [DefTree] -> String -> Assertion
assertContains tree definitionPath =
    unless
        (definitionPath `existsIn` tree)
        (assertFailure $ "Expected to find " ++ show definitionPath ++
                         " in " ++ show tree)

isUnique :: Ord a => [a] -> Bool
isUnique xs = length xs == S.size (S.fromList xs)
