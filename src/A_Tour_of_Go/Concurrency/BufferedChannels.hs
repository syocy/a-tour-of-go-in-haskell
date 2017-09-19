module A_Tour_of_Go.Concurrency.BufferedChannels where

import Control.Concurrent.BoundedChan (BoundedChan, newBoundedChan, writeChan, readChan)
import Control.Concurrent.STM (atomically, TBQueue, newTBQueue, readTBQueue, writeTBQueue)

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

-- |
-- >>> mainBySTM
-- 1
-- 2
-- 3
mainBySTM :: IO ()
mainBySTM = do
  ch <- atomically $ newTBQueue 2 :: IO (TBQueue Int)
  atomically $ writeTBQueue ch 1
  atomically $ writeTBQueue ch 2
  print =<< atomically (readTBQueue ch)
  print =<< atomically (readTBQueue ch)
  atomically $ writeTBQueue ch 3
  print =<< atomically (readTBQueue ch)
