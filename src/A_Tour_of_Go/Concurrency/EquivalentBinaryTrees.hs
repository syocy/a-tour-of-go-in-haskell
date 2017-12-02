module A_Tour_of_Go.Concurrency.EquivalentBinaryTrees where

import A_Tour_of_Go.Concurrency.Tree

import Control.Concurrent
  (Chan, newChan, writeChan, readChan, getChanContents)
import Control.Concurrent.Async (async)
import Control.Monad (forever, forM, forM_)
import Data.Maybe (catMaybes, isJust)

walk :: Tree -> Chan (Maybe Int) -> IO ()
walk tree ch = walk' tree >> writeChan ch Nothing
  where
    walk' Nil = return ()
    walk' (Tree v l r) = do
      walk' l
      writeChan ch $ Just v
      walk' r

same :: Tree -> Tree -> IO Bool
same t1 t2 = do
  ch1 <- newChan
  ch2 <- newChan
  async $ walk t1 ch1
  async $ walk t2 ch2
  loop ch1 ch2
    where
      loop ch1 ch2 = do
        x1 <- readChan ch1
        x2 <- readChan ch2
        if x1 /= x2
          then return False
          else
            if x1 == Nothing || x2 == Nothing
              then return True
              else loop ch1 ch2

-- |
-- >>> main
-- 1,2,3,4,5,6,7,8,9,10,
-- True
-- False
main :: IO ()
main = do
  ch <- newChan
  tree1 <- newTree 1
  async $ walk tree1 ch
  let extractJust = catMaybes . takeWhile isJust
  ch' <- fmap extractJust $ getChanContents ch
  forM_ ch' $ \x -> putStr (show x ++ ",")
  putStrLn ""
  tree1' <- newTree 1
  tree2 <- newTree 2
  print =<< same tree1 tree1'
  print =<< same tree1 tree2

walkPure :: Tree -> [Int]
walkPure Nil = []
walkPure (Tree v l r) = walkPure l ++ [v] ++ walkPure r

samePure :: Tree -> Tree -> Bool
samePure t1 t2 = walkPure t1 == walkPure t2

-- |
-- >>> mainPure
-- 1,2,3,4,5,6,7,8,9,10,
-- True
-- False
mainPure :: IO ()
mainPure = do
  tree1 <- newTree 1
  forM_ (walkPure tree1) $ \x -> putStr (show x ++ ",")
  putStrLn ""
  tree1' <- newTree 1
  tree2 <- newTree 2
  print $ samePure tree1 tree1'
  print $ samePure tree1 tree2
