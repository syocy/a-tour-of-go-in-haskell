module A_Tour_of_Go.Concurrency.Tree (Tree(..), newTree, shuffle) where

import Prelude hiding (Either(..))
import System.Random.MWC
import qualified Data.Vector as V
import Data.List (sort)

data Tree = Nil | Tree Int Tree Tree deriving(Show)

size :: Int
size = 10

shuffle :: (Ord a) => [Int] -> [a] -> [a]
shuffle rnds xs = map snd $ sort $ zip rnds xs

insert :: Int -> Tree -> Tree
insert x Nil = Tree x Nil Nil
insert x (Tree y l r) | x < y     = Tree y (insert x l) r
                      | otherwise = Tree y l (insert x r)

newTree' :: [Int] -> Int -> Tree
newTree' rnds x = foldl (flip insert) Nil $ shuffle rnds $ map (x*) [1..size]

newTree :: Int -> IO Tree
newTree x = do
  rnds <- withSystemRandom . asGenIO $ \gen -> uniformVector gen size
  return $ newTree' (V.toList rnds) x
