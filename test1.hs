module Main where
-- import Data.List hiding (reverse)
-- import qualified Data.String as S
import qualified Data.List as L
import qualified Data.Set as Set
import Data.Either
import Test.QuickCheck
-- import qualified Data.ByteString as B

-- XXX: Example of a simple Int evaluator
data IAst = I Double
          | Add IAst IAst
          | S String
          | Sub IAst IAst deriving (Ord, Eq, Show)

-- XXX: Arbitary generator
instance Arbitrary IAst where
  arbitrary = do
            x <- arbitrarySizedFractional
            z <- getString
            elements [I x, Add (I x) (I x), Sub (I x) (I x), S z]
            where
                getString :: Gen [Char]
                getString = listOf $ arbitraryBoundedRandom

instance Bounded IAst where
  minBound = I 0
  maxBound = S ""



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

        
-- Fizz Buzz test in Haskell
fizzBuzz :: IO ()
fizzBuzz = do 
         print (map fizzbuzz [1..100])
         where
              fizzbuzz x = case (x `mod` 3 == 0, x `mod` 5 == 0) of
                                (True,True) -> "FizzBuzz"
                                (True,False) -> "Fizz"
                                (False,True) -> "Buzz"
                                (False,False) -> show x
                                
mapi     :: (Num i) => (i -> a -> a) -> [a] -> [a]
mapi f x = map' 0 x
     where
       map' _ [] = []
       map' c (x1:xs) = (f c x1) : (map' (c + 1) xs)
       
map2       :: (a -> b -> c) -> [a] -> [b] -> Either String [c]
map2 f a b = 
     if L.length a == L.length b 
     then Right (map2' a b) 
     else Left "Strings not of equal length"
     where
          map2' [] []         = []
          map2' (a:as) (b:bs) = (f a b) : map2' as bs
          map2' _ _           = error "Sr" -- XXX: What is the point of this??
          
map2Shortest                 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2Shortest _ [] []         = []
map2Shortest _ [] _          = []
map2Shortest _ _ []          = []
map2Shortest f (x:xs) (y:ys) = (f x y) : map2Shortest f xs ys

apply :: (a -> a -> a) -> [a] -> Either String a
apply _ [] = Left "List is empty"
apply f (x:xs) = Right (L.foldl' f x xs)

-- XXX: Example of a closure in haskell
plusx :: (Num a) => a -> (a -> a)
plusx x =
  let y = 3
  in (+ x*y)

map3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
map3 _ [] _ _ = []
map3 _ _ [] _ = []
map3 _ _ _ [] = []
map3 f (a:as) (b:bs) (c:cs) = (f a b c) : map3 f as bs cs

data BTree a = BLeaf a
              | BNode a (BTree a) (BTree a) deriving (Eq, Show, Ord)

instance Functor BTree where
  fmap f (BLeaf g) = BLeaf (f g)
  fmap f (BNode a y z) = BNode (f a) (fmap f y) (fmap f z)
  
llookup :: (Ord a) => a -> BTree a -> Bool
llookup v (BNode x y z) = if x == v 
                      then True
                      else if v < x 
                      then llookup v y
                      else llookup v z
llookup v (BLeaf x) = v == x
  
instance Applicative BTree where
  pure x  = BLeaf x
  (BLeaf f) <*> (BLeaf x) = BLeaf (f x)
  (BNode f f1 f2) <*> (BNode x y z) = BNode (f x) (f1 <*> y) (f2 <*> z)
  
class (Traversable t) => Stack t where
  pop  :: t a -> (Maybe a, t a) 
  push :: a -> t a -> t a

instance Stack [] where
  pop (x:xs) = (Just x, xs)
  pop []     = (Nothing, [])
  push x y   = x:y


qsort :: (Ord a, Eq a) => [a] -> [a]
qsort [] = [] 
qsort (x:xs) = (qsort (L.filter (< x) xs)) ++ [x] ++ (qsort (L.filter (> x) xs))

  
newtype F = F Integer

-- XXX: foldr
mfoldr :: (b -> a -> b) -> b -> [a] -> b
mfoldr _ x [] = x
mfoldr f x (y:ys) = f (mfoldr f x ys) y

-- XXX: foldl
mfoldl            :: (a -> b -> b) -> [a] -> b -> b
mfoldl _ [] y     = y
mfoldl f (x:xs) y = mfoldl f xs (f x y)

-- XXX: Adjacency list data structure for a graph
newtype GNode a = GNode [a] deriving (Show, Eq)
newtype AdjList a = AdjList [GNode a] deriving (Show, Eq)
    
-- XXX: Example of Quickcheck

main :: IO ()
-- Fill this in later
main = do 
     -- B.putStr $ S.fromString "Please enter a lisp expression to evaluate: "
     -- expression <- B.getLine
     -- B.putStrLn expression
     putStrLn $ show (b + b)
