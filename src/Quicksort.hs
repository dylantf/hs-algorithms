module Quicksort (quicksortNaive, quicksortST) where

import Control.Monad.ST (runST)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV

quicksortNaive :: (Ord a) => [a] -> [a]
quicksortNaive [] = []
quicksortNaive [x] = [x]
quicksortNaive (pivot : rest) =
  quicksortNaive [x | x <- rest, x <= pivot]
    ++ [pivot]
    ++ quicksortNaive [x | x <- rest, x > pivot]

quicksortST :: (Ord a) => [a] -> [a]
quicksortST [] = []
quicksortST [x] = [x]
quicksortST xs = runST $ do
  vec <- V.thaw (V.fromList xs)
  go vec 0 (MV.length vec - 1)
  V.toList <$> V.freeze vec
  where
    go vec lo hi
      | lo >= hi = pure ()
      | otherwise = do
          pivotIdx <- partition vec lo hi
          go vec lo (pivotIdx - 1)
          go vec (pivotIdx + 1) hi
    partition vec lo hi = do
      pivot <- MV.read vec hi
      let loop i j
            | j >= hi = do
                MV.swap vec i hi
                pure i
            | otherwise = do
                val <- MV.read vec j
                if val < pivot
                  then do
                    MV.swap vec i j
                    loop (i + 1) (j + 1)
                  else
                    loop i (j + 1)
      loop lo lo
