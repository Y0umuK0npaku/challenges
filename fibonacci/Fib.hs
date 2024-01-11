{-# LANGUAGE BangPatterns #-}

module Fibonacci (fib) where

import Data.List
import System.IO (withFile, IOMode(WriteMode), hPutStrLn)
import Control.Exception (assert)
import Data.Bits (testBit)

fib :: Integer -> Integer
fib n
  | n >= 0    = snd . foldl_ fib_ (1, 0) . dropWhile not $ [testBit n k | k <- [numBits - 1, numBits - 2 .. 0]]
  | otherwise = (-1) ^ fromInteger (-n + 1) * fib (-n)
  where
    fib_ (f, g) p
      | p = (f * (f + 2*g), ss)
      | otherwise = (ss, g * (2*f - g))
      where
        ss = f*f + g*g

    foldl_ = foldl'

    numBits = integerLog2 n + 1

    integerLog2 :: Integer -> Int
    integerLog2 = go 0
      where
        go acc 0 = acc - 1
        go acc x = go (acc + 1) (x `div` 2)


main :: IO ()
main = do
  nums <- fmap parseInts getContents
  let results = zipWith (\x r -> (x, r)) nums [fib x | x <- nums]
  withFile "output.txt" WriteMode $ \file -> do
    mapM_ (hPutStrLn file . formatResult) results



formatResult :: (Integer, Integer) -> String
formatResult (x, result) = "k: " ++ show x ++ ", result: " ++ show result

parseInts :: String -> [Integer]
parseInts str = map read (words str)