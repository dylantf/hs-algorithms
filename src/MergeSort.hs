module MergeSort (mergeSort, mergeSortST) where

import Control.Monad (when)
import Control.Monad.ST
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort items =
  let (left, right) = split items
   in merge (mergeSort left) (mergeSort right)
  where
    split :: [a] -> ([a], [a])
    split [] = ([], [])
    split [x] = ([x], [])
    split xs = splitAt (length xs `div` 2) xs

    merge :: (Ord a) => [a] -> [a] -> [a]
    merge xs [] = xs
    merge [] ys = ys
    merge (x : xs) (y : ys)
      | x <= y = x : merge xs (y : ys)
      | otherwise = y : merge (x : xs) ys

mergeSortST :: (Ord a) => [a] -> [a]
mergeSortST [] = []
mergeSortST [x] = [x]
mergeSortST items = runST $ do
  let vec = V.fromList items
      n = V.length vec
  src <- V.thaw vec
  tmp <- MV.new n
  go src tmp 0 n
  V.toList <$> V.freeze src
  where
    go src tmp lo hi
      | hi - lo <= 1 = pure ()
      | otherwise = do
          let mid = lo + (hi - lo) `div` 2
          go src tmp lo mid
          go src tmp mid hi
          mergeInto src tmp lo mid hi

    mergeInto src tmp lo mid hi = do
      forRange lo hi $ \i ->
        MV.read src i >>= MV.write tmp (i - lo)

      let loop i j k
            | i >= mid - lo && j >= hi - lo = pure ()
            | i >= mid - lo = do
                MV.read tmp j >>= MV.write src k
                loop i (j + 1) (k + 1)
            | j >= hi - lo = do
                MV.read tmp i >>= MV.write src k
                loop (i + 1) j (k + 1)
            | otherwise = do
                a <- MV.read tmp i
                b <- MV.read tmp j
                if a <= b
                  then MV.write src k a *> loop (i + 1) j (k + 1)
                  else MV.write src k b *> loop i (j + 1) (k + 1)
      loop 0 (mid - lo) lo

    forRange lo hi action = go' lo
      where
        go' i = when (i < hi) $ action i *> go' (i + 1)