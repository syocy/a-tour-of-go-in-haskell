module A_Tour_of_Go.Concurrency.BufferedChannels where

-- import Control.Concurrent.STM (atomically, TBQueue, newTBQueue, readTBQueue, writeTBQueue)
import Control.Concurrent.BoundedChan (BoundedChan, newBoundedChan, writeChan, readChan)

-- |
-- >>> main
-- 1
-- 2
-- 3
main :: IO ()
main = do
  ch <- newBoundedChan 2 :: IO (BoundedChan Int)
  writeChan ch 1
  writeChan ch 2
  print =<< readChan ch
  print =<< readChan ch
  writeChan ch 3
  print =<< readChan ch
