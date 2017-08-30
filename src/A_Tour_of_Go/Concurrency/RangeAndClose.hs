module A_Tour_of_Go.Concurrency.RangeAndClose where

import Control.Concurrent.BoundedChan (BoundedChan, newBoundedChan, writeChan, readChan, getChanContents)
import Control.Concurrent.Async (async)
import Control.Monad (forM_, foldM_)
import Data.Maybe (catMaybes, isJust)

fibonacci :: Int -> BoundedChan (Maybe Int) -> IO ()
fibonacci n ch = do
  foldM_ inner (0, 1) [0..(n-1)]
  writeChan ch Nothing
    where
      inner :: (Int, Int) -> Int -> IO (Int, Int)
      inner (x, y) _ = do
        writeChan ch $ Just x
        return (y, x + y)

range :: BoundedChan (Maybe a) -> IO [a]
range ch = catMaybes . takeWhile isJust <$> getChanContents ch

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
