module A_Tour_of_Go.Concurrency.Goroutines where

import Control.Concurrent.Async (async)
import Control.Concurrent (threadDelay)
import Control.Monad (forM_)

say :: String -> IO ()
say s = forM_ [0..4] $ \i -> do
  threadDelay $ 100 * 10^3
  putStrLn s

main :: IO ()
main = do
  async $ say "world"
  say "hello"
