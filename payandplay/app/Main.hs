module AbstractionElimination (eliminate) where
import Control.Monad
import Control.Monad.Trans
import System.Console.Haskeline
import Data.Char (isAlphaNum, isDigit)
import qualified Text.Parsec.String as ParsecString
import qualified Text.Parsec.Token as Tok
import Text.Parsec (Parsec, parse, ParseError, eof, (<|>), try, many1, sepBy1, between, char, spaces, digit, satisfy)
import Text.Parsec.Language (haskellStyle)
import Text.Parsec.String (Parser)
Import Pretty (ppexpr)

type Name = String

data Expr
  = Var Name
  | Lit Lit
  | App Expr Expr
  | Lam Name Expr
  deriving (Eq, Show)

data Lit
  = LInt Int
  | LBool Bool
  deriving (Show, Eq, Ord)

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where ops = ["->","\\","+","*","-","="]
        names = []
        style = haskellStyle {Tok.reservedOpNames = ops,
                              Tok.reservedNames = names,
                              Tok.commentLine = "#"}

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

natural :: Parser Integer
natural = Tok.natural lexer

variable :: Parser Expr
variable = do
  x <- identifier
  return (Var x)


lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  args <- many1 identifier
  reservedOp "."
  body <- expr
  return $ foldr Lam body args

term :: Parser Expr
term =  parens expr
    <|> variable
    <|> lambda

expr :: Parser Expr
expr = do
  es <- many1 term
  return (foldl1 App es)
  

convertComplex :: Expr -> String
convertComplex (Var x) = x
convertComplex (Lit _) = ""  -- Ignore literals or use a placeholder
convertComplex (App e1 e2) = "(" ++ convertComplex e1 ++ " " ++ convertComplex e2 ++ ")"
convertComplex (Lam x e) = convertLambda x e

parseExpr :: String -> Either ParseError Expr
parseExpr input = parse (contents expr) "<stdin>" input

convertLambda :: Name -> Expr -> String
convertLambda x (Var y)
    | x == y = "I"
    | otherwise = "K " ++ y
convertLambda x (App e1 e2) = "S (" ++ convertLambda x e1 ++ ") (" ++ convertLambda x e2 ++ ")"
convertLambda x (Lam y e) 
    | x == y = "I"
    | y `notElem` freeVars e = "K (" ++ convertLambda x e ++ ")"
    | otherwise = "S (" ++ convertLambda x (Lam y e1) ++ ") (" ++ convertLambda x e2 ++ ")"
    where
        (e1, e2) = case e of
            App e1 e2 -> (e1, e2)
            _ -> (e, Var y) -- Default case for non-application expressions


freeVars :: Expr -> [Name]
freeVars (Var x) = [x]
freeVars (Lit _) = []
freeVars (App e1 e2) = freeVars e1 ++ freeVars e2
freeVars (Lam x e) = filter (/= x) (freeVars e)


eliminate :: String -> String
eliminate str = case parseExpr str of
    Left err -> "Error: " ++ show err
    Right expr -> convertComplex expr

showStep :: (Int, Expr) -> IO ()
showStep (d, x) = putStrLn ((replicate d ' ') ++ "=> " ++ ppexpr x)


process :: String -> IO ()
process line = do
  let res = parseExpr line
  case res of
    Left err -> print err
    Right ex -> do
      let (out, ~steps) = runEval ex
      mapM_ showStep steps
      print out

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "Untyped> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> (liftIO $ process input) >> loop