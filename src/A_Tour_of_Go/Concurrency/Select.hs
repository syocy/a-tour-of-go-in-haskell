module A_Tour_of_Go.Concurrency.Select where

import Control.Concurrent.STM (TQueue, newTQueue, readTQueue, writeTQueue)
import Control.Monad.STM (STM, atomically, orElse)
import Control.Monad (forM_)
import Control.Concurrent.Async (async)

select :: [STM a] -> IO a
select ms = atomically $ foldr1 inner ms
  where
    inner :: STM a -> STM a -> STM a
    inner q m = m `orElse` q

fibonacci :: TQueue Int -> TQueue Int -> IO ()
fibonacci ch quit = go 0 1
  where
    go x y = do
      selected <- select [ writeTQueue ch x >> return (Just (y, x+y))
                         , readTQueue quit >> return Nothing
                         ]
      case selected of
        Just (x', y') -> go x' y'
        Nothing -> putStrLn "quit"

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
-- quit
main :: IO ()
main = do
  ch <- atomically $ newTQueue
  quit <- atomically $ newTQueue
  async $ do
    forM_ [0..9] $ \_ -> do
      x <- atomically $ readTQueue ch
      print x
    atomically $ writeTQueue quit 0
  fibonacci ch quit
