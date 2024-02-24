module Main where 

import Text.Printf (printf)
import Control.Monad (unless)
import Data.Either (isLeft, fromRight)

data Operation = Plus 
  | Minus 
  | Mult 
  | Div 
  | Pow 

instance Show Operation where 
  show Plus = " + "
  show Minus = " - "
  show Mult = " * "
  show Div = " / "
  show Pow  = " ^ "

instance Eq Operation where 
  Plus == Plus = True
  Plus == _ = False
  Minus == Minus = True
  Minus == _ = False
  Mult == Mult = True
  Mult == _ = False
  Div == Div = True
  Div == _ = False
  Pow == Pow = True
  Pow == _ = False

data Expr = 
  Const Double
  | Bin Expr Operation Expr
  | Sqrt Expr

instance Show Expr where 
  show (Const a) = show a
  show (Sqrt exp) = printf " sqrt( %s )" (show exp)
  show (Bin exp1 oper exp2) = show exp1 ++ show oper ++ show exp2

instance Eq Expr where 
  Const a == Const b = a == b
  Sqrt exp1 == Sqrt exp2 = exp1 == exp2
  Bin a1 op1 b1 == Bin a2 op2 b2 = op1 == op2 && a1 == a2 && b1 == b2
  -- (==) = undefined 

data Error = DivByZero [Char]
  | NegativeRoot [Char] 


instance Show Error where 
  show (DivByZero msg) = printf "ERROR_DIV_BY_ZERO: %s / 0" msg
  show (NegativeRoot msg) = printf "ERROR_NEGATIVE_ROOT: %s" msg

instance Eq Error where 
  DivByZero msg1 == DivByZero msg2 = msg1 == msg2
  NegativeRoot msg1 == NegativeRoot msg2 = msg1 == msg2
  -- (==) = undefined 

eval :: Expr -> Either Error Double 
eval (Const a) = Right a

eval (Sqrt exp) = 
  let ans = eval exp in 
    if isLeft ans then ans else 
      let val = fromRight 1 ans in 
        if val < 0 then Left $ NegativeRoot $ show val 
        else Right $ sqrt val

eval (Bin exp1 op exp2) 
  | op == Plus = resolveOperation exp1 (+) exp2
  | op == Minus = resolveOperation exp1 (-) exp2
  | op == Mult = resolveOperation exp1 (*) exp2
  | op == Div = let ans1 = eval exp1 in
    let ans2 = eval exp2 in
    if isLeft ans1 then ans1 else 
      if isLeft ans2 then ans2 else
        if fromRight 1 ans2 == 0 then Left $ DivByZero $ show $ fromRight 1 ans1
        else resolveOperation exp1 (/) exp2
  | op == Pow = resolveOperation exp1 (**) exp2

resolveOperation :: Expr -> (Double -> Double -> Double) -> Expr -> Either Error Double
resolveOperation exp1 op exp2 =
  let ans1 = eval exp1 in
    let ans2 = eval exp2 in
    if isLeft ans1 then ans1 else 
      if isLeft ans2 then ans2 else
        Right $ fromRight 1 ans1 `op` fromRight 1 ans2


cases :: [(Expr, Either Error Double)]
cases = [
  (Sqrt $ Const 1.0, Right 1.0),
  (Bin (Sqrt (Const 9.0)) Plus (Bin (Const 2) Pow (Const 7.0)), Right 131.0),
  (Sqrt (Const (-1)), Left $ NegativeRoot "-1.0"),
  (Bin (Bin (Const 7.0) Mult (Const 2.0)) Div (Bin (Const 4.0) Minus (Const 4.0)), Left $ DivByZero "14.0")
  ]

test :: Expr -> Either Error Double -> IO () 
test expr expected = 
    let actual = eval expr in 
    unless (expected == actual) $ describeFailure actual
  where 
    describeFailure actual = 
      printf "eval (%s) should be %s but it was %s" (show expr) (show expected) (show actual) 
  

main :: IO () 
main = do 
  mapM_ (uncurry test) cases 
  