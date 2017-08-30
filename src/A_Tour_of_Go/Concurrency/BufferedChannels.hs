module A_Tour_of_Go.Concurrency.BufferedChannels where

import Control.Concurrent.STM (atomically, TBQueue, newTBQueue, readTBQueue, writeTBQueue)

-- |
-- >>> main
-- 1
-- 2
-- 3
main :: IO ()
main = do
  c <- atomically $ newTBQueue 2 :: IO (TBQueue Int)
  atomically $ do
    writeTBQueue c 1
    writeTBQueue c 2
  print =<< atomically (readTBQueue c)
  print =<< atomically (readTBQueue c)
  atomically $ writeTBQueue c 3
  print =<< atomically (readTBQueue c)
