module A_Tour_of_Go.Concurrency.ExerciseWebCrawler where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (Async, async, wait, cancel, withAsync, forConcurrently_)
import           Control.Concurrent.STM ( atomically
                                        , TVar, newTVar, modifyTVar', readTVar
                                        , TQueue, newTQueue, writeTQueue, readTQueue
                                        )
import           Control.DeepSeq (deepseq)
import           Control.Monad (forM_, forM, when, forever)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set

data FetchResult = Fetched { fetchedBody :: String, fetchedUrls :: [String] }
                 | FetchError String
                 deriving (Show)

class Fetcher a where
  fetch :: a -> String -> IO FetchResult

crawlNaive :: (Fetcher f) => String -> Int -> f -> IO ()
crawlNaive url depth f | depth <= 0 = return ()
                       | otherwise  = do
  fetchResult <- fetch f url
  case fetchResult of
    FetchError mes -> putStrLn mes
    Fetched body urls -> do
      putStrLn $ "found: " ++ url ++ " \"" ++ body ++ "\""
      forM_ urls $ \url -> do
        crawlNaive url (depth - 1) f

crawl :: (Fetcher f) => String -> Int -> f -> IO ()
crawl url depth f = do
  cache <- atomically $ newTVar Set.empty
  retQueue <- atomically $ newTQueue
  a <- startOutputThread retQueue
  crawl' url depth f cache retQueue
  threadDelay $ 10 * 10^3 -- wait last output
  cancel a

startOutputThread :: TQueue String -> IO (Async ())
startOutputThread retQueue = async $ forever $ do
  str <- atomically $ readTQueue retQueue
  putStrLn str

crawl' :: (Fetcher f) => String -> Int -> f -> TVar (Set String) -> TQueue String -> IO ()
crawl' url depth f cache retQueue | depth <= 0 = return ()
                                  | otherwise  = do
  reserved <- tryReserveUrl url cache
  when reserved $ do
    fetchResult <- fetch f url
    case fetchResult of
      FetchError mes -> atomically $ writeTQueue retQueue mes
      Fetched body subUrls -> do
        atomically $ writeTQueue retQueue $ "found: " ++ url ++ " \"" ++ body ++ "\""
        forConcurrently_ subUrls $ \subUrl -> do
          crawl' subUrl (depth - 1) f cache retQueue

tryReserveUrl :: String -> TVar (Set String) -> IO Bool
tryReserveUrl url cache = atomically $ do
  noKey <- (not . Set.member url) `fmap` readTVar cache
  when noKey $ do
    modifyTVar' cache $ Set.insert url
  return noKey

-- |
-- >>> main
-- found: http://golang.org/ "The Go Programming Language"
-- found: http://golang.org/pkg/ "Packages"
-- not found: http://golang.org/cmd/
-- found: http://golang.org/pkg/fmt/ "Packages fmt"
-- found: http://golang.org/pkg/os/ "Packages os"
main :: IO ()
main = do
  -- crawlNaive "http://golang.org/" 4 fetcher
  crawl "http://golang.org/" 4 fetcher

data FakeResult = FakeResult
                  { fakeBody :: String
                  , fakeUrls :: [String]
                  } deriving (Show)

newtype FakeFetcher = FakeFetcher { unFakeFetcher :: Map String FakeResult }

instance Fetcher FakeFetcher where
  fetch f url = case Map.lookup url (unFakeFetcher f) of
    Nothing -> return $ FetchError $ "not found: " ++ url
    Just (FakeResult body urls) -> return $ Fetched body urls

fetcher :: FakeFetcher
fetcher = FakeFetcher $ Map.fromList
          [ ( "http://golang.org/"
            , FakeResult
              "The Go Programming Language"
              [ "http://golang.org/pkg/"
              , "http://golang.org/cmd/"
              ]
            )
          , ( "http://golang.org/pkg/"
            , FakeResult
              "Packages"
              [ "http://golang.org/"
              , "http://golang.org/cmd/"
              , "http://golang.org/pkg/fmt/"
              , "http://golang.org/pkg/os/"
              ]
            )
          , ( "http://golang.org/pkg/fmt/"
            , FakeResult
              "Packages fmt"
              [ "http://golang.org/"
              , "http://golang.org/pkg/"
              ]
            )
          , ( "http://golang.org/pkg/os/"
            , FakeResult
              "Packages os"
              [ "http://golang.org/"
              , "http://golang.org/pkg/"
              ]
            )
          ]
