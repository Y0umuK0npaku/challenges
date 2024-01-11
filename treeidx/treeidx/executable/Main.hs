module Main (main) where
import System.IO (withFile, IOMode(WriteMode), hPutStrLn)
import Data.List (permutations)
import SubTreeLabeling(subtreeCount, generateTree)


main :: IO ()
main = do
  content <- getContents
  let inputs = map ((\[k, d] -> (k, d)) . map read . words) . lines $ content
  let results = zipWith (\(k, d) r -> (k, d, r)) inputs [subtreeCount (generateTree k d) | (k, d) <- inputs]
  withFile "output.txt" WriteMode $ \file -> do
    mapM_ (hPutStrLn file . formatResult) results

formatResult :: (Int, Int, Integer) -> String
formatResult (k, d, result) = "k: " ++ show k ++ ", d: " ++ show d ++ ", result: " ++ show result
