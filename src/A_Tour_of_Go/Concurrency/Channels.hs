module A_Tour_of_Go.Concurrency.Channels where

import Control.Concurrent (Chan, newChan, readChan, writeChan)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM
  (atomically, TQueue, newTQueue, writeTQueue, readTQueue)
import Prelude hiding (sum)
import qualified Data.List as L

sum :: [Int] -> Chan Int -> IO ()
sum xs chan = do
  let ret = L.sum xs
  writeChan chan ret

-- |
-- >>> main
-- (-5,17,12)
main :: IO ()
main = do
  let s = [7, 2, 8, -9, 4, 0]
  c <- newChan
  async $ sum (drop (length s `div` 2) s) c
  async $ sum (take (length s `div` 2) s) c
  [x, y] <- sequence [readChan c, readChan c] -- sequence run a list of actions
  print (x, y, x+y)

{- STM version -}

sumBySTM :: [Int] -> TQueue Int -> IO ()
sumBySTM xs chan = do
  let ret = L.sum xs
  atomically $ writeTQueue chan ret

-- |
-- >>> mainBySTM
-- (-5,17,12)
mainBySTM :: IO ()
mainBySTM = do
  let s = [7, 2, 8, -9, 4, 0]
  c <- atomically $ newTQueue
  async $ sumBySTM (drop (length s `div` 2) s) c
  async $ sumBySTM (take (length s `div` 2) s) c
  [x, y] <- sequence [atomically (readTQueue c), atomically (readTQueue c)]
  print (x, y, x+y)
