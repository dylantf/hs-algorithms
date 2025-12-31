module MergeSort (mergeSort) where

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
