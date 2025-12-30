module Main where

import Bubble (bubbleNaive, bubbleST)

bubble :: IO ()
bubble = do
  let items = [5, 4, 3, 7, 1, 2, 3, 19, 42, 41, 40, 29, 10, 17] :: [Int]
  putStrLn $ "Unordered: " ++ show items
  putStrLn $ "Naive: " ++ show (bubbleNaive items)
  putStrLn $ "ST:" ++ show (bubbleST items)

main :: IO ()
main = do
  bubble
