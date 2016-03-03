module Main where

data BTree a = Leaf a
                | Node a (BTree a) (BTree a) deriving (Show, Eq)

data MyList a =
    Nil
    | Cons a (MyList a) deriving (Show, Eq)
      
tree2list              :: BTree a -> [a]
tree2list (Leaf a)     = [a]
tree2list (Node a l r) = a : ((tree2list l) ++ (tree2list r))

addList              :: MyList a -> MyList a -> MyList a
addList Nil x        = x
addList x Nil        = x
addList (Cons a b) x = Cons a (addList b x)

printList            :: (Show a) => MyList a -> String
printList Nil        = ""
printList (Cons a b) = (show a) ++ printList b
                       
tutu :: (Num a, Fractional a) => a
tutu = 4.03
       
main :: IO ()
main = do 
     let x = Cons 3 (Cons 4 Nil) ;
     print (printList x);
     return ()
            
-- Writing the funtor for MyList
instance Functor MyList where
    fmap _ Nil = Nil
    fmap f (Cons a b) = Cons (f a) (fmap f b)
    x <$ _ = Cons x Nil

instance Applicative MyList where
    pure x = Cons x Nil
    Cons x y <*> Cons a b = Cons (x a) (y <*> b)
    _ <*> _ = Nil
    _ *> y = y
    x <* _ = x
 
instance Monoid (MyList a) where
    mempty = Nil
    mappend (Cons x xs) y = Cons x (mappend xs y)
    mappend Nil y = y

instance Foldable MyList where
    foldMap f (Cons x xs)  = f x `mappend` foldMap f xs
    foldMap _ Nil  = mempty
    foldr f x (Cons y ys)  = foldr f (f y x) ys
    foldr _ x Nil  = x

-- Writing the Monad for the list
instance Monad MyList where
    Cons x xs >>= f = (f x) `mappend` (xs >>= f)
    Nil >>= _ = Nil

