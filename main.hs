module Main where

perms :: [t] -> [[t]] -- Any lists, not only Eq!
perms zs = map (\z -> xi z zs) (perms' (ix zs)) where

 ix xx = ix' [] 0 xx where
  ix' r i [] = reverse r
  ix' r i (x:xs) = ix' (i:r) (i+1) xs

 xi ixs xs = xi' [] ixs xs where
  xi' r [] _ = reverse r
  xi' r (ix:ixs) xs = xi' ((geti ix xs):r) ixs xs 
  geti 0 (x:xs) = x
  geti i (x:xs) = geti (i-1) xs

 perms' [] = [[]]
 perms' xs = concatMap (\x -> concatMap (\y -> [x : y]) (perms' (filter (/=x) xs))) xs
