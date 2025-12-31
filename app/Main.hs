module Main where

import Bubble (bubbleNaive, bubbleST)
import MergeSort (mergeSort)
import Quicksort (quicksortNaive, quicksortST)

items :: [Int]
items = [5, 4, 3, 7, 1, 2, 3, 19, 42, 41, 40, 29, 10, 17]

bubble :: IO ()
bubble = do
  putStrLn $ "Unordered: " ++ show items
  putStrLn $ "Naive: " ++ show (bubbleNaive items)
  putStrLn $ "ST:" ++ show (bubbleST items)

quicksort :: IO ()
quicksort = do
  putStrLn $ "Unordered: " ++ show items
  putStrLn $ "Naive: " ++ show (quicksortNaive items)
  putStrLn $ "ST: " ++ show (quicksortST items)

ms :: IO ()
ms = do
  putStrLn $ "Unordered: " ++ show items
  putStrLn $ "Merge sort: " ++ show (mergeSort items)

main :: IO ()
main = do
  ms
