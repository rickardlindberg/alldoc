module Fixtures where

import Control.Monad
import DocPiece
import Test.QuickCheck

instance Arbitrary DocPiece where
    arbitrary = resize 10 $ sized tree
        where
            tree 0 = leaf
            tree n = oneof [ leaf , node (n `div` 2) ]
            leaf   = liftM2 DocPiece  (return "") (return "")
            node n = liftM3 Namespace (return "") (return "") (listOf (tree n))
