module Main where
import Test.QuickCheck
import Prelude hiding (reverse)

reverse :: [a] -> [a]
reverse [] = [] 
reverse (x:xs) = reverse xs ++ [x]  

-- XXX: Now some quick-check property declaration
rev_rev :: [Int]  -> [Int] -> Bool
rev_rev x y = reverse (x++y) == reverse y ++ reverse x 

main :: IO ()
main = quickCheck rev_rev
