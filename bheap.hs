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
swap f x (Node y1 y2 y3) = if y1 == x
                              then (Node y1 y2 y3)
                              else 
                               let left            = swap f x y2
                                   right           = swap f x y3
                               in 
                                case (left, right) of
                                  (Empty, Empty) -> Node y1 left right
                                  (Empty, Node v l r) -> if not (f x v)
                                                            then Node v left (Node y1 l r)
                                                            else Node y1 left right
                                  (Node v l r, Empty) -> if not (f x v)
                                                            then Node v (Node y1 l r) right
                                                            else Node y1 left right
                                  (Node vl ll rl, Node vr lr rr) -> if not (f x vl) && not (f x vr)
                                                                       then error "Screwed"
                                                                       else if not (f x vl) 
                                                                            then  Node vl (Node y1 ll rl) right
                                                                               else if not (f x vr) 
                                                                                    then Node vr (Node y1 lr rr) left
                                                                                       else Node y1 left right



-- XXX: Insert
insert       :: (Ord a) => (a -> a -> Bool) -> a -> Bheap a -> Bheap a
insert f x y = insert' y x ->- swap f x
