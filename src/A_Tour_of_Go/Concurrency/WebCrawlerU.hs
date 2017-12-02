module A_Tour_of_Go.Concurrency.WebCrawlerU where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
  ( Async, async, wait, cancel
  , withAsync, forConcurrently_ )
import Control.Concurrent.STM
  ( atomically
  , TVar, newTVar, modifyTVar', readTVar
  , TQueue, newTQueue, writeTQueue, readTQueue )
import Control.DeepSeq (deepseq)
import Control.Monad (forM_, forM, when, forever)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

data FetchResult
  = Fetched { fetchedBody :: String, fetchedUrls :: [String] }
  | FetchError String
  deriving (Show)

class Fetcher a where
  fetch :: a -> String -> IO FetchResult

{- `crawlNaive` is a naive implementation of `crawl`. -}
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

{- Replace implementation of `crawl` to remove url duplications. -}
crawl :: (Fetcher f) => String -> Int -> f -> IO ()
crawl url depth f = crawlNaive url depth f

-- |
-- >>> main
-- found: http://golang.org/ "The Go Programming Language"
-- found: http://golang.org/pkg/ "Packages"
-- found: http://golang.org/ "The Go Programming Language"
-- found: http://golang.org/pkg/ "Packages"
-- not found: http://golang.org/cmd/
-- not found: http://golang.org/cmd/
-- found: http://golang.org/pkg/fmt/ "Packages fmt"
-- found: http://golang.org/ "The Go Programming Language"
-- found: http://golang.org/pkg/ "Packages"
-- found: http://golang.org/pkg/os/ "Packages os"
-- found: http://golang.org/ "The Go Programming Language"
-- found: http://golang.org/pkg/ "Packages"
-- not found: http://golang.org/cmd/
main :: IO ()
main = do
  crawl "http://golang.org/" 4 fetcher

data FakeResult = FakeResult
                  { fakeBody :: String
                  , fakeUrls :: [String]
                  } deriving (Show)

newtype FakeFetcher
  = FakeFetcher { unFakeFetcher :: Map String FakeResult }

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
