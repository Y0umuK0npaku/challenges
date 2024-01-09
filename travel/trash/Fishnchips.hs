module Fishnchips (main) where

import Lib
import Control.Monad (replicateM)


solveCase :: [Int] -> [Int] -> Int
solveCase [n, x] a = let distances = zipWith (-) (a ++ [x, x]) (0:a ++ [last a])
			 lastDistance = (x - last a) * 2
                         maxDistance = maximum distances
                     in max maxDistance lastDistance
                     
readCase :: IO ([Int], [Int])
readCase = do
  nx <- fmap (map read . words) getLine
  a <- fmap (map read . words) getLine
  return (nx, a)

main :: IO ()
main = do
  t <- readLn :: IO Int
  cases <- replicateM t readCase
  let results = [solveCase nx a | (nx, a) <- cases]
  mapM_ print results
