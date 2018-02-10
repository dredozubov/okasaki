module Exercise_2_1 where

-- 2.1
suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes (x:xs) = (x:xs) : suffixes xs
