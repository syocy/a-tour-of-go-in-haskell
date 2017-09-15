module A_Tour_of_Go.Concurrency.ExerciseWebCrawler where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad (forM_)

type Error = Maybe String

class Fetcher a where
  fetch :: a -> String -> IO (String, [String], Error)

crawl :: (Fetcher f) => String -> Int -> f -> IO ()
crawl url depth f | depth <= 0 = return ()
                  | otherwise  = do
  (body, urls, err) <- fetch f url
  case err of
    Just mes -> putStrLn mes
    Nothing -> do
      putStrLn $ "found: " ++ url ++ " \"" ++ body ++ "\""
      forM_ urls $ \url -> do
        crawl url (depth - 1) f

-- |
-- >>> main
-- 0
main :: IO ()
main = do
  crawl "http://golang.org/" 4 fetcher

data FakeResult = FakeResult
                  { body :: String
                  , urls :: [String]
                  } deriving (Show)

newtype FakeFetcher = FakeFetcher { unFakeFetcher :: Map String FakeResult }

instance Fetcher FakeFetcher where
  fetch f url = case Map.lookup url (unFakeFetcher f) of
    Nothing -> return ("", [], Just $ "not found" ++ url)
    Just result -> return (body result, urls result, Nothing)

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
