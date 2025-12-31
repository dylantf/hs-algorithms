Sorting algorithms in Haskell

Each implemented in "naive" way (linked lists, creating temporary lists, etc) and in ST monad for more efficient swapping in place.

# Benchmarks

### Randomized list

| Algorithm   | n         | Naive  | ST     |
| ----------- | --------- | ------ | ------ |
| Bubble Sort | 10,000    | 1.847s | 0.281s |
| Quick Sort  | 1,000,000 | 0.908s | 0.196s |
| Merge Sort  | 1,000,000 | 1.830s | 0.232s |

### Already sorted list

| Algorithm  | n       | Naive  | ST     |
| ---------- | ------- | ------ | ------ |
| Quick Sort | 10,000  | 0.574s | 0.221s |
| Merge Sort | 100,000 | 0.038s | 0.008s |

notes:

Bubble sort exits early on the first pass, not included here

Quicksort on an already sorted list using the last item at a pivot essentially freezes at 100k elements, so I had to bump it down to 10k
