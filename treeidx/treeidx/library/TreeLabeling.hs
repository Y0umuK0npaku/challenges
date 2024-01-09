module TreeLabeling (factorial, countValidLabelings, totalNodes) where
 
import Data.List (tails)
import Data.List ((\\))


totalNodes :: Int -> Int -> Integer
totalNodes k d = sum [toInteger k^i | i <- [0..d]]


factorial :: Integer -> Integer
factorial n
   | n == 0 = 1
   | otherwise = n * factorial (n-1)

countValidLabelings :: Int -> Int -> Integer
countValidLabelings k d
    | d == 0 = 1
    | d == 1 = factorial $ fromIntegral k
    | k == 1 = 1
    | otherwise = factorial (n - 1) * (countValidLabelings k (d - 1))^k `div` (factorial (nkd))^k
    where
      n = totalNodes k d
      nkd = totalNodes k (d - 1)

      