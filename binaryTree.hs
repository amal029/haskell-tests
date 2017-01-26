{-# LANGUAGE GADTs #-}
{-# LANGUAGE  FlexibleInstances #-}

module BinaryTree where
import Test.QuickCheck
import Control.Monad


data BinaryTree a where
     Leaf :: a -> BinaryTree a
     Node :: a -> BinaryTree a -> BinaryTree a -> BinaryTree a
                                                             deriving (Show, Eq, Ord)
                                                             

instance Arbitrary (BinaryTree Int) where
  arbitrary = oneof [liftM Leaf (arbitrary :: Gen Int)
                    ,liftM3 Node (arbitrary :: Gen Int) (arbitrary :: Gen (BinaryTree Int)) 
                                                        (arbitrary :: Gen (BinaryTree Int))]

-- The map function on the binary tree
tmap :: BinaryTree a -> (a -> a) -> BinaryTree a
tmap (Leaf x) f = Leaf (f x)
tmap (Node x1 x2 x3) f = Node (f x1) (tmap x2 f) (tmap x3 f)
  
                                                 

-- Inorder traversal of the tree with fold
tfold :: a -> BinaryTree a -> (a -> a -> a) -> a
tfold x (Leaf t) f = f x t
tfold x (Node v t2 t3) f = tfold (f (tfold x t2 f) v) t3 f

-- main :: IO ()
-- main = 
--   let
--     tree = Node 2 (Leaf 1) (Leaf 3)
--   in 
--     print (tfold (0 :: Integer) tree (-))
