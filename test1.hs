module Main where
-- import Data.List hiding (reverse)
-- import qualified Data.String as S
import qualified Data.List as L
import qualified Data.Set as Set
import Data.Either
-- import qualified Data.ByteString as B

-- An ast for a lisp evaluator
  
data Ast a = Literal a 
           | BinOp Op (Ast a) (Ast a) deriving (Show, Eq)

data Op = Plus
              | Minus
              | Div
              | Mult deriving (Show, Eq)
              

-- Write the evaluator
eval                   :: (Fractional a) => Ast a -> a
eval (Literal x)       = x
eval (BinOp Plus x y)  = (eval x) + (eval y)
eval (BinOp Minus x y) = (eval x) - (eval y)
eval (BinOp Mult x y)  = (eval x) * (eval y)
eval (BinOp Div x y)   = (eval x) / (eval y)

-- Phantom types in Haskell
newtype Value a = Value Integer deriving (Show, Eq)
data Mile = Mile deriving (Show, Eq)
data KM = KM deriving (Show, Eq)

a :: Value Mile
a = Value 10

b :: Value KM
b = Value 100

instance Num (Value a) where
    (Value x) + (Value y) = Value (x + y)
    (Value x) - (Value y) = Value (x - y)
    (Value x) * (Value y) = Value (x * y)
    negate (Value x) = Value (negate x)
    abs (Value x) = Value (abs x)
    signum (Value x) = Value (signum x)
    fromInteger x = Value x
    
-- Partitioning the balls
    
data Balls = Red
           | Blue deriving (Show, Eq)
             
getBalls :: [Balls] -> ([Balls], Int)
getBalls x =  
  let (p1,p2) = L.partition (\y -> case y of
                                         Red -> True
                                         _ -> False) x in
  (p1++p2, L.length p1)
  
  
getMissingElement :: (Int, Int) -> [Int] -> Int
getMissingElement (s, e) x =
  let s1 = Set.fromList [s..e] in
  let l2 = Set.fromList x in
  (head . Set.toList) $ Set.difference s1 l2
  
  
-- Frequnency of elements in a list
freqList :: (Ord a) => [a] -> [a]
freqList x = 
  let res = (L.group . L.sort) x in
  let res2 = zip (L.map (\y -> length y) res) res in
  let (_,y) = unzip $ L.sortOn (\(s, _) -> s) res2 in
  concat y
  
data BSTree a = Leaf a
                | Node a (BSTree a) (BSTree a) deriving (Show, Eq)

-- reconstruct the binary tree pre-order traversal
bTreePreOrder :: (Ord a) => [a] -> Either String (BSTree a)
bTreePreOrder [] = Left "Cannot build tree!" 
bTreePreOrder rest = 
  if L.length rest < 3 || L.length rest `mod` 3 /= 0
  then Left "Cannot build tree!" 
  else myfunc rest
  where
    myfunc [] = Left "Cannot build tree!"
    myfunc (x: []) = Right (Leaf x)
    myfunc (x:rest2) = 
        if L.length rest2 == 2
        then Right (Node x (Leaf (rest2 !! 0)) (Leaf (rest2 !! 1)))
        else 
            let rmax = L.foldl1 (\x1 y -> x1 `max` y) rest2
                (ll, rl) = L.span (< rmax) rest2
                llr = myfunc ll
                rrr = myfunc rl
                ret = case (isLeft llr, isLeft rrr) of
                            (True, _) -> llr
                            (_, True) -> rrr
                            (False, False) -> do
                                    llv <- llr
                                    rrv <- rrr
                                    pure (Node x llv rrv)
            in ret

bTreeInOrder :: (Ord a) => [a] -> Either String (BSTree a) 
bTreeInOrder [] = Left "Cannot build tree!" 
bTreeInOrder rest = 
  if L.length rest < 3 || L.length rest `mod` 3 /= 0
     then Left "Cannot build tree!"
     else myfunc rest
     where
        myfunc [] = Left "Cannot build tree!"
        myfunc (x: []) = Right (Leaf x)
        myfunc (x:rest2) = 
          if L.length rest2 == 2
          then Right (Node (rest2 !! 0) (Leaf x) (Leaf (rest2 !! 1)))
          else 
               let rmax = L.foldl1 (\x1 y -> x1 `max` y) (x:rest2)
                   (ll, rl) = L.span (< rmax) (x:rest2)
                   hd = L.head rl
                   lll = (myfunc ll)
                   rrr = (myfunc $ L.tail rl)
                   ret = case (isRight lll, isRight rrr) of
                              (True, True) -> do
                                           llv <- lll
                                           rrv <- rrr
                                           pure (Node hd llv rrv)
                              (False,_) -> lll
                              (_, False) -> rrr
               in ret


main :: IO ()
-- Fill this in later
main = do 
     -- B.putStr $ S.fromString "Please enter a lisp expression to evaluate: "
     -- expression <- B.getLine
     -- B.putStrLn expression
     putStrLn $ show (b + b)
