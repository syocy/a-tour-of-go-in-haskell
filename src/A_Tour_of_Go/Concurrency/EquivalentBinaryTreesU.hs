module A_Tour_of_Go.Concurrency.EquivalentBinaryTreesU where

import A_Tour_of_Go.Concurrency.Tree

import Control.Concurrent
  (Chan, newChan, writeChan, readChan, getChanContents)
import Control.Concurrent.Async (async)
import Control.Monad (forever, forM, forM_)
import Data.Maybe (catMaybes, isJust)

{- `walk` walk a tree in ascending order -}
{- and push each element to a channel.   -}
walk :: Tree -> Chan (Maybe Int) -> IO ()
walk tree ch = undefined

{- `same` check whether two trees are equivalent or not. -}
same :: Tree -> Tree -> IO Bool
same t1 t2 = undefined

main :: IO ()
main = undefined

{- Pure version -}

walkPure :: Tree -> [Int]
walkPure Nil = []
walkPure (Tree v l r) = walkPure l ++ [v] ++ walkPure r

samePure :: Tree -> Tree -> Bool
samePure t1 t2 = walkPure t1 == walkPure t2

mainPure :: IO ()
mainPure = do
  tree1 <- newTree 1
  forM_ (walkPure tree1) $ \x -> putStr (show x ++ ",")
  putStrLn ""
  tree1' <- newTree 1
  tree2 <- newTree 2
  print $ samePure tree1 tree1'
  print $ samePure tree1 tree2
