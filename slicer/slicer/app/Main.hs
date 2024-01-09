module Main (main) where

import Lib
import Data.Map as Map


findRemainingNumber :: Int -> Int -> Int
findRemainingNumber n m = findPowerAndOffset 0 0 + n 
  where
    findPowerAndOffset :: Int -> Int -> Int
    findPowerAndOffset power offset
        | 2 ^ power > (m - n - 1) = offset
        | otherwise = findPowerAndOffset (power + 2) (2 ^ power + offset)

main = do
    n <- readLn :: IO Int
    m <- readLn :: IO Int
    let range = [n..m]  -- Adjust the range as needed for testing
    let results = Prelude.map (\m -> (findRemainingNumber n m, m)) range
    let uniqueResults = Map.fromListWith min results
    if 0 < n && n < m
    then mapM_ print (Map.toList uniqueResults)
    else return ()
