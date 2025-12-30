module Bubble
  ( bubbleNaive,
    bubbleST,
  )
where

import Control.Monad.ST
import Data.STRef
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV

-- Naive implementation without mutating
bubbleNaive :: (Ord a) => [a] -> [a]
bubbleNaive xs =
  let (maybeSorted, performedSwap) = bubblePass xs
   in if performedSwap then bubbleNaive maybeSorted else maybeSorted
  where
    bubblePass :: (Ord a) => [a] -> ([a], Bool)
    bubblePass [] = ([], False)
    bubblePass [x] = ([x], False)
    bubblePass (x : y : rest)
      | x > y = let (rest', _) = bubblePass (x : rest) in (y : rest', True)
      | otherwise = let (rest', s) = bubblePass (y : rest) in (x : rest', s)

-- "Proper" bubble sort mutating the list as we iterate
bubbleST :: (Ord a) => [a] -> [a]
bubbleST xs = runST $ do
  vec <- V.thaw (V.fromList xs)
  swapped <- newSTRef False
  let n = MV.length vec
  let outerLoop 0 = return ()
      outerLoop i = do
        writeSTRef swapped False
        innerLoop 0 i
        didSwap <- readSTRef swapped
        if didSwap
          then outerLoop (i - 1)
          else return ()
      innerLoop j limit
        | j >= limit - 1 = return ()
        | otherwise = do
            a <- MV.read vec j
            b <- MV.read vec (j + 1)
            if a > b
              then do
                MV.swap vec j (j + 1)
                writeSTRef swapped True
              else
                return ()
            innerLoop (j + 1) limit
  outerLoop n
  V.toList <$> V.freeze vec
