module SymbolicDifferentiationOfPrefixExpressions (diff) where

data Expr = Num Int
          | Var String
          | BinOp String Expr Expr 
          | UnOp String Expr       
          deriving (Eq, Show)


parseExpr :: String -> Expr
parseExpr s
  | not (null s) && head s == '(' && last s == ')' = parseExpr (tail (init s))
  | all (`elem` ['0'..'9']) s = Num (read s)
  | otherwise = let (op, rest) = break (== ' ') s
                    (a, b) = parseArgs (drop 1 rest)
                in case b of
                     "" -> Var op  -- Handle variables
                     _  -> BinOp op (parseExpr a) (parseExpr b)  
  where
    parseArgs s = let (a, rest) = break (== ' ') s
                      b = dropWhile (== ' ') rest
                  in (a, b)



deriv :: Expr -> Expr
deriv (Num _) = Num 0
deriv (Var _) = Num 1
deriv (BinOp "+" a b) = BinOp "+" (deriv a) (deriv b)
deriv (BinOp "-" a b) = BinOp "-" (deriv a) (deriv b)
deriv (BinOp "*" a b) = BinOp "+" (BinOp "*" (deriv a) b) (BinOp "*" a (deriv b))
deriv (BinOp "/" a b) = BinOp "/" (BinOp "-" (BinOp "*" (deriv a) b) (BinOp "*" a (deriv b))) (BinOp "^" b (Num 2))
deriv (BinOp "^" a b) = case b of
    Num n -> BinOp "*" (BinOp "*" (Num n) (BinOp "^" a (Num (n - 1)))) (deriv a)
    _     -> BinOp "*" (BinOp "^" a b) (deriv (BinOp "*" b (UnOp "ln" a)))
deriv (UnOp "exp" a) = BinOp "*" (deriv a) (UnOp "exp" a)
deriv (UnOp "sin" a) = BinOp "*" (deriv a) (UnOp "cos" a)
deriv (UnOp "cos" a) = BinOp "*" (deriv a) (BinOp "*" (Num (-1)) (UnOp "sin" a))
deriv (UnOp "tan" a) = BinOp "*" (deriv a) (BinOp "+" (Num 1) (BinOp "^" (UnOp "tan" a) (Num 2)))
deriv (UnOp "ln" a) = BinOp "/" (deriv a) a

simplify :: Expr -> Expr
simplify (BinOp "+" (Num a) (Num b)) = Num (a + b)
simplify (BinOp "-" (Num a) (Num b)) = Num (a - b)
simplify (BinOp "*" (Num a) (Num b)) = Num (a * b)
simplify (BinOp "/" (Num a) (Num b)) = Num (a `div` b)  -- Assuming integer division
simplify (BinOp "^" (Num a) (Num b)) = Num (a ^ b)

simplify (BinOp "*" a b)
  | a == Num 0 || b == Num 0 = Num 0
  | a == Num 1 = simplify b
  | b == Num 1 = simplify a
  | otherwise  = BinOp "*" (simplify a) (simplify b)

simplify (BinOp "+" a b)
  | a == Num 0 = simplify b
  | b == Num 0 = simplify a
  | otherwise  = BinOp "+" (simplify a) (simplify b)

simplify (BinOp "-" a b)
  | b == Num 0 = simplify a
  | otherwise  = BinOp "-" (simplify a) (simplify b)

simplify (BinOp "/" a b)
  | b == Num 0 = error "Division by zero"
  | b == Num 1 = simplify a
  | otherwise  = BinOp "/" (simplify a) (simplify b)

simplify (BinOp op a b) = BinOp op (simplify a) (simplify b)
simplify (UnOp op a) = UnOp op (simplify a)
simplify x = x

exprToString :: Expr -> String
exprToString (Num n) = show n
exprToString (Var x) = x
exprToString (BinOp op a b) = op ++ " " ++ exprToString a ++ " " ++ exprToString b
exprToString (UnOp op a) = op ++ " " ++ exprToString a

diff :: String -> String
diff = exprToString . simplify . deriv . parseExpr