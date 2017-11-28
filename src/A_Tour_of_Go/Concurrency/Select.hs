module A_Tour_of_Go.Concurrency.Select where

import Control.Concurrent.STM
  ( STM, atomically, orElse
  , TQueue, newTQueue, readTQueue, writeTQueue )
import Control.Monad (forM_, join)
import Control.Concurrent.Async (async)

select :: [STM a] -> IO a
select stms = atomically $ foldl1 orElse stms

fibonacci :: TQueue Int -> TQueue () -> IO ()
fibonacci ch quit = loop 0 1
  where
    loop x y = join $ select
      [ readTQueue quit  >> return (print "quit")
      , writeTQueue ch x >> return (loop y (x+y))
      ]

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
-- "quit"
main :: IO ()
main = do
  ch <- atomically $ newTQueue
  quit <- atomically $ newTQueue
  async $ do
    forM_ [0..9] $ \_ -> do
      x <- atomically $ readTQueue ch
      print x
    atomically $ writeTQueue quit ()
  fibonacci ch quit
