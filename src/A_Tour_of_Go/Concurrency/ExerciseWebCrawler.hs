module A_Tour_of_Go.Concurrency.ExerciseWebCrawler where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad (forM_, forM, when)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Concurrent.Async (async, wait, withAsync, forConcurrently_)
import Control.Concurrent.STM ( atomically
                              , TVar, newTVar, modifyTVar', readTVar
                              , TQueue, newTQueue, writeTQueue, readTQueue)
import Control.DeepSeq (deepseq)

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
  crawl' url depth f cache retQueue
  --

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
        -- as <- forM subUrls $ \subUrl -> do
        --   print subUrl
        --   async $ do
        --     print subUrl
        --     crawl' subUrl (depth - 1) f cache
        -- forM_ as wait

tryReserveUrl :: String -> TVar (Set String) -> IO Bool
tryReserveUrl url cache = atomically $ do
  noKey <- (not . Set.member url) `fmap` readTVar cache
  when noKey $ do
    modifyTVar' cache $ Set.insert url
  return noKey

-- |
-- >>> main
-- 0
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
