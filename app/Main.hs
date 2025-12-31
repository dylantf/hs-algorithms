module Main where

import Bubble (bubbleNaive, bubbleST)
import Control.Exception (evaluate)
import Data.Time.Clock
import MergeSort (mergeSort, mergeSortST)
import Quicksort (quicksortNaive, quicksortST)
import System.Random

items :: [Int]
items = [5, 4, 3, 7, 1, 2, 3, 19, 42, 41, 40, 29, 10, 17]

makeBigList :: IO [Int]
makeBigList = take 1_000_000 . randomRs (1, 1_000_000) <$> newStdGen

time :: String -> IO a -> IO a
time label action = do
  start <- getCurrentTime
  result <- action
  end <- getCurrentTime
  putStrLn $ label ++ ": " ++ show (diffUTCTime end start)
  return result

main :: IO ()
main = do
  bigList <- makeBigList
  let smallList = take 10000 bigList
  let bigSortedList = [1 .. 100000] :: [Int]
  let smallSortedList = take 10000 bigSortedList

  putStrLn $ "\n-- Bubble Sort --"
  _ <- time "bubbleNaive (list)" $ evaluate $ length (bubbleNaive smallList)
  _ <- time "bubbleST (mutable)" $ evaluate $ length (bubbleST smallList)

  putStrLn "\n-- Quick Sort --"
  _ <- time "quicksortNaive (list)" $ evaluate $ length (quicksortNaive smallSortedList)
  _ <- time "quicksortST (mutable)" $ evaluate $ length (quicksortST smallSortedList)

  putStrLn "\n-- Merge Sort --"
  _ <- time "mergeSort (list)" $ evaluate $ length (mergeSort bigSortedList)
  _ <- time "mergeSortST (mutable)" $ evaluate $ length (mergeSortST bigSortedList)

  pure ()