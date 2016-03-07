module Bheap where

    
(->-)     :: a -> (a -> a) -> a
(->-) x f = f x

data Bheap a = Empty
            | Node a (Bheap a) (Bheap a) deriving (Show, Eq)


              
-- XXX: Make and empty heap
empty :: Bheap a
empty = Empty

-- XXX: Insert a node into the Bheap
insert'                   :: (Ord a) => Bheap a -> a -> Bheap a
insert' Empty z           = Node z Empty Empty
insert' (Node x1 x2 x3) z = case (x2, x3) of
                                 (Empty, _)                -> Node x1 (insert' x2 z) x3
                                 (_, Empty)                -> Node x1 x2 (insert' x3 z)
                                 (Node _ _ _, Node _ _ _ ) -> let d2 = level x2
                                                                  d3 = level x3
                                                              in 
                                                                 if d2 <= d3
                                                                 then Node x1 (insert' x2 z) x3
                                                                 else Node x1 x2 (insert' x3 z)
                                                                 
                            where
                              -- XXX: Get the level of the tree
                              level   :: Bheap a -> Integer
                              level x = level' x 0
                                  where
                                    level'                  :: Bheap a -> Integer -> Integer
                                    level' Empty c          = c
                                    level' (Node _ x22 x33) c = level' x22 (c + 1) `min` level' x33 (c + 1)



-- XXX: Ensure the heap property
swap                     :: (Ord a) => (a -> a -> Bool) -> a -> Bheap a -> Bheap a
swap _ _ Empty           = Empty
swap f x (Node y1 y2 y3) = let lookup = undefined
                           in undefined


-- XXX: Insert
insert       :: (Ord a) => (a -> a -> Bool) -> a -> Bheap a -> Bheap a
insert f x y = insert' y x ->- swap f x
