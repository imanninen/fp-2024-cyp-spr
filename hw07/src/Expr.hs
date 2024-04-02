module Expr where 
import StateDemo ( State, execState, get, modify )
import Data.Maybe ( fromJust )
import Error (Error(..))

data BinOpeation =
  Plus |
  Minus |
  Mult |
  Div |
  Pow deriving (Show, Eq)

data UnoOperation = 
  Sqrt deriving (Show, Eq)

data Expr a = Var String |
  Const a |
  Uno UnoOperation (Expr a) |
  Bin BinOpeation (Expr a) (Expr a) deriving (Show, Eq)
        
type ExprState a = [(String, a)]

performeOperation:: (a -> a -> a) -> Either Error a -> Either Error a -> Either Error a
performeOperation op a b =
  case (a, b) of 
    (Left er, _) -> Left er
    (_, Left er) -> Left er
    (Right x, Right y) -> Right $ op x y

eval ::(Ord a, Floating a, Show a) => Expr a -> State (ExprState a) (Either Error a)
eval (Var v) = do 
  env <- get 
  return $ Right $ fromJust $ lookup v env


eval (Const x) = return $ Right x 

eval (Bin Plus x y) = do 
  x <- eval x 
  y <- eval y
  return $ performeOperation (+) x y

eval (Bin Minus x y) = do 
  x <- eval x 
  y <- eval y
  return $ performeOperation (-) x y

eval (Bin Mult x y) = do 
  x <- eval x 
  y <- eval y
  return $ performeOperation (*) x y

eval (Bin Div x y) = do 
  x <- eval x 
  y <- eval y
  case (x, y) of 
    (Left er, _) -> return $ Left er
    (_, Left er) -> return $ Left er
    (Right a, Right 0) -> return $ Left $ DivByZero $ show a
    (Right a, Right b) -> return $ Right $ a / b

eval (Bin Pow x y) = do 
  x <- eval x 
  y <- eval y
  case (x, y) of 
    (Left er, _) -> return $ Left er
    (_, Left er) -> return $ Left er
    (Right 0, Right 0) -> return $ Left $ ZeroPowZero "0 ^ 0"
    (Right a, Right b) -> return $ Right $ a ** b

-- eval (Let x v b) = do 
--   v <- eval v 
--   case v of 
--     Left err -> return $ Left err
--     Right a -> finish where
--       finish = do 
--         modify ((x, a) :)
--         eval b 

eval (Uno Sqrt exp) = do
  res <- eval exp
  case res of
    Left err -> return $ Left err
    Right a -> 
      if a < 0 
        then return $ Left $ NegativeRoot $ show a 
      else 
        return $ Right $ sqrt a

runEval :: (Ord a, Floating a, Show a) => Expr a -> Either Error a
runEval expr = 
  execState (eval expr) [] 