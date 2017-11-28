module A_Tour_of_Go.Concurrency.DefaultSelection where

import Control.Concurrent.STM
  ( STM, atomically, orElse
  , TQueue, newTQueue, writeTQueue, readTQueue )
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel)
import Control.Monad (forever, join)

select :: [STM a] -> IO a
select stms = atomically $ foldl1 orElse stms

newTicker :: Int -> IO (TQueue (), IO ())
newTicker microSec = do
  ch <- atomically newTQueue
  thread <- async $ forever $ do
    threadDelay microSec
    async $ atomically $ writeTQueue ch ()
  return (ch, cancel thread)

newAfter :: Int -> IO (TQueue (), IO ())
newAfter microSec = do
  ch <- atomically newTQueue
  thread <- async $ do
    threadDelay microSec
    atomically $ writeTQueue ch ()
  return (ch, cancel thread)

-- |
-- >>> main
--     .
--     .
-- tick.
--     .
--     .
-- tick.
--     .
--     .
-- tick.
--     .
--     .
-- tick.
--     .
--     .
-- BOOM!
main :: IO ()
main = do
  (tick, _) <- newTicker (100 * 10^3)
  (boom, _) <- newAfter (500 * 10^3)
  loop tick boom
    where
      loop tick boom = do
        join $ select
          [ readTQueue boom >> return (do
              putStrLn "BOOM!")
          , readTQueue tick >> return (do
              putStrLn "tick."
              loop tick boom)
          , return (do
              putStrLn "    ."
              threadDelay (51 * 10^3)
              loop tick boom)
          ]
