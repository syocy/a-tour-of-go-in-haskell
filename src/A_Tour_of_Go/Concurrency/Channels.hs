-- |
-- Module : A_Tour_of_Go.Concurrency.Channels

module A_Tour_of_Go.Concurrency.Channels
  ( sum
  , main
  ) where

import Control.Concurrent (Chan, newChan, readChan, writeChan)
import Control.Concurrent.Async (async)
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
  [x, y] <- sequence [readChan c, readChan c]
  print (x, y, x+y)
