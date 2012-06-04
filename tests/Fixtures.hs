module Fixtures where

import Control.Monad
import Definitions
import Test.QuickCheck

instance Arbitrary DefTree where
    arbitrary = resize 10 $ sized tree
        where
            tree 0     = leaf
            tree n     = oneof [ leaf , node (n `div` 2) ]
            leaf       = liftM2 Definition (return "") (return "")
            node n     = liftM3 Namespace (nsName n) (return "") (subNodes n)
            nsName n   = return $ "ns" ++ show n
            subNodes n = do
                x <- listOf (tree n)
                return $ map prefix (zip [1..length x] x)
            prefix (n, Namespace a b c) = Namespace (show n ++ a) b c
            prefix (_, x)               = x
    shrink (Definition _ _)   = []
    shrink (Namespace _ _ []) = []
    shrink (Namespace a b _)  = [Namespace a b []]
