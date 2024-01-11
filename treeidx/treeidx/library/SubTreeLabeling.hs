module SubTreeLabeling (generateTree, subtreeCount, totalNodes) where
import TreeLabeling (factorial)
import Data.List (tails)
import Data.List ((\\))



data Tree a = Node {
    rootLabel :: a,          
    subForest :: [Tree a]    
} deriving (Show)

generateTree :: Int -> Int -> Tree Int
generateTree k 0 = Node 1 []
generateTree k d = Node 1 [generateTree k (d-1) | _ <- [1..k]]

totalNodes :: Tree a -> Integer
totalNodes (Node _ subTrees) = 1 + sum (map totalNodes subTrees)


subtreeCount :: Tree a -> Integer
subtreeCount tree@(Node _ subTrees)
  | null subTrees = 1
  | otherwise = let k = toInteger $ length subTrees
                    n = totalNodes tree
                    nkd = totalNodes (head subTrees)
                in factorial (n - 1) * (subtreeCount (head subTrees))^k `div` (factorial nkd)^k

main = do
  content <- getContents
  let inputs = map ((\[k, d] -> (k, d)) . map read . words) . lines $ content
  let trees = [generateTree k d | (k, d) <- inputs]
  let results = map subtreeCount trees
  mapM_ print results


