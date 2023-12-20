module Remove where

remove :: [Int] -> [Int] -> [Int]
remove xs ys = filter (`notElem` ys) xs