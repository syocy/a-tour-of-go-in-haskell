module A_Tour_of_Go.Concurrency.SyncMutex where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (async)
import           Control.Concurrent.STM ( atomically
                                        , TVar, newTVar, modifyTVar', readTVar
                                        )
import           Control.Monad (replicateM_)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data SafeCounter = SafeCounter { v :: TVar (Map String Int) }

newSafeCounter :: Map String Int -> IO SafeCounter
newSafeCounter m = atomically $ SafeCounter `fmap` newTVar m

inc :: SafeCounter -> String -> IO ()
inc c key = atomically $ do
  modifyTVar' (v c) $ Map.alter inc' key
    where
      inc' Nothing = Just 1
      inc' (Just x) = Just (x + 1)

value :: SafeCounter -> String -> IO Int
value c key = atomically $ do
  Map.findWithDefault 0 key `fmap` readTVar (v c)

-- |
-- >>> main
-- 1000
main :: IO ()
main = do
  c <- newSafeCounter $ Map.empty
  replicateM_ 1000 $ do
    async $ inc c "somekey"
  threadDelay $ 10^6
  print =<< value c "somekey"
