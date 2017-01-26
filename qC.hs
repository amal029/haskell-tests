module QC where


import BinaryTree as BT
import Test.QuickCheck

-- A property of a list
prop_rev :: [Int] -> Bool
prop_rev x = reverse x == x
  
  
prop_tmap :: BT.BinaryTree Int -> Bool
prop_tmap t = tmap t (* 2) == t


-- main :: IO ()
-- main = return ()  
