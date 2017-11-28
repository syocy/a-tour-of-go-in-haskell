module A_Tour_of_Go.Concurrency.RangeAndClose where

import Control.Concurrent.BoundedChan
  ( BoundedChan, newBoundedChan, writeChan, readChan
  , getChanContents )
import Control.Concurrent.Async (async)
import Control.Monad (forM_, foldM_)
import Data.Maybe (catMaybes, isJust)

range :: BoundedChan (Maybe a) -> IO [a]
range ch = fmap extractJust $ getChanContents ch
  where
    extractJust = catMaybes . takeWhile isJust

fibonacci :: Int -> BoundedChan (Maybe Int) -> IO ()
fibonacci n ch = do
  foldM_ step (0, 1) [0..(n-1)]
  writeChan ch Nothing
    where
      step :: (Int, Int) -> Int -> IO (Int, Int)
      step (x, y) _ = do
        writeChan ch $ Just x
        return (y, x + y)

-- |
-- >>> main
-- 0
-- 1
-- 1
-- 2
-- 3
-- 5
-- 8
-- 13
-- 21
-- 34
main :: IO ()
main = do
  let size = 10
  ch <- newBoundedChan size
  async $ fibonacci size ch
  ch' <- range ch
  forM_ ch' $ \i -> do
    print i
  return ()

{- List version -}

fibonacciByList :: Int -> [Int]
fibonacciByList n = fib n 0 1
  where
    fib 0 a _ = []
    fib n a b = a : fib (n-1) b (a + b)

-- |
-- >>> mainByList
-- 0
-- 1
-- 1
-- 2
-- 3
-- 5
-- 8
-- 13
-- 21
-- 34
mainByList :: IO ()
mainByList = do
  let size = 10
  let fibs = fibonacciByList size
  forM_ fibs $ \i -> do
    print i
  return ()
