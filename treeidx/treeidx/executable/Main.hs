module Main (main) where

import Data.List (permutations)
import TreeLabeling (countValidLabelings, factorial, totalNodes)

-- Include your Tree definition, generateKaryTree, labelCombinationsForLevel, countValidLabelings functions here

main :: IO ()
main = do
  content <- getContents
  let inputs = map ((\[k, d] -> (k, d)) . map read . words) . lines $ content
  let results = [countValidLabelings k d | (k, d) <- inputs]
  mapM_ print results
