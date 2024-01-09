module SubTreeLabeling (generateTree, subtreeCount, totalNodes) where
import Data.List (tails)
import Data.List ((\\))



data Tree a = Node {
    rootLabel :: a,          
    subForest :: [Tree a]    
} deriving (Show)


generateTree :: Integer -> Integer -> Tree Integer
generateTree k 0 = Node 1 []
generateTree k d = Node 1 [generateTree k (d-1) | _ <- [1..k]]

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)

-- Count the total number of nodes in the tree
totalNodes :: Tree a -> Integer
totalNodes (Node _ subTrees) = 1 + sum (map totalNodes subTrees)

-- Calculate the number of valid labelings for a k-ary tree
subtreeCount :: Tree Integer -> Integer
subtreeCount tree@(Node _ subTrees)
  | null subTrees = 1
  | otherwise = let k = toInteger $ length subTrees
                    n = totalNodes tree
                    nkd = totalNodes (head subTrees)
                in fact (n - 1) `div` product [fact nkd ^ k | _ <- subTrees]


