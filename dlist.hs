module Main where
import Data.STRef
import Control.Monad.ST
import Control.Monad()
import Debug.Trace


data Node s a = Node
                    { value :: a
                    , next :: STRef s (Node s a)
                    } 
                    | Nil deriving (Eq)


-- instance of show for Node s a
instance (Show a) => Show (Node s a) where
  show Nil = "{}"
  show (Node v n) = "{" ++ (show v) ++ "-->" ++ (traverseEdges n) ++ "}"
    where
        traverseEdges x = do
                        -- t <- readSTRef x
                        show $ readSTRef x

-- Insert a node in the list
minsert       :: (Show a) => STRef s (Node s a) -> Node s a -> ST s ()
minsert h n   = do
        hv <- readSTRef h
        traceShow ("inside: " ++ "hv: " ++ (show hv) ++ ", n: " ++ (show n)) $ case hv of
          Nil        -> writeSTRef h (ppn n)
          Node _ ref -> (minsert ref n)
          where
               ppn _ = case n of
                           Nil -> traceShow "n is Nil: " $ n
                           _ -> traceShow "ref of n is not Nil!" $ n

mlength   :: (Show a) => STRef s (Node s a) -> ST s Integer
mlength h = do 
        getCount 0 h
    where
        getCount c h1 = traceShow "inside mlength" $ do
                   hv <- readSTRef h1
                   traceShow ("hv: "++ (show hv)) $  case hv of
                                   Nil        -> pure c
                                   Node _ ref -> getCount (c + 1) ref


mdelete         :: STRef s (Node s a) -> Integer -> ST s (Either String (Node s a))
mdelete h todel = delete' 0 h Nil
                  where
                    delete'' c p = 
                      case p of
                        Nil        -> pure (Right c)
                        Node _ ref -> do
                                      _ <- modifySTRef' ref (\_ -> c)
                                      x <- readSTRef h
                                      pure (Right x)
                    delete' c' h' prev = do
                        hv <- readSTRef h'
                        case hv of
                            Nil        -> pure (Left "Nothing to delete")
                            Node _ ref -> if c' == todel
                                              then do
                                                   rv <- readSTRef ref
                                                   delete'' rv prev
                                              else delete' (c' + 1) ref hv

main :: IO ()
main = do 
  runST $ testinsert start
  where
    start :: Node s Integer
    start = Nil
    testinsert h = do
            s <- newSTRef h
            z <- newSTRef Nil
            _ <- readSTRef z
            y1 <- readSTRef s
            _ <- pure (print $ show y1)
            mapM_ (\y -> traceShow "calling minsert " $ minsert s (Node y z)) [1..1]
            s3 <- mdelete s 0
            case s3 of
              Left s1 -> pure (print $ show s1)
              Right s2 -> do
                       tt <- newSTRef s2
                       ii <- mlength tt
                       pure (print $ show ii)
                       -- pure (print $ show s2)
