module MyExpr.Simplify where

-- import MyExpr.Expr (Expr(..), BinOpeation(..), UnoOperation(..), ExprState)
-- import MyExpr.StateDemo ( State, execState, get, modify )

-- simplify :: (Eq a, Fractional a, Ord a) => Expr a ->  State (ExprState a) (Expr a)
-- simplify expr =
--   case expr of
--     Const a -> Const a
--     Var x -> Var x
--     -- Plus - *
--     Bin Plus exp (Const 0.0) -> simplify exp
--     Bin Plus (Const 0.0) exp -> simplify exp
--     Bin Plus exp1 exp2 -> Bin Plus (simplify exp1) (simplify exp2)
--     -- Minus - *
    
--     Bin Minus exp (Const 0.0) -> simplify exp
--     Bin Minus (Const 0.0) (Const a) -> Const ((-1) * a)
--     -- Bin exp Minus (Const a) -> if a < 0 then Bin (simplify exp) Plus (Const ((-1) * a)) else Bin (simplify exp) Minus (Const a)
--     Bin Minus exp1 exp2 -> if exp1 == exp2 then Const 0 else Bin Minus (simplify exp1) (simplify exp2)
--     -- Mult - *
--     Bin Mult (Const 1.0) exp -> simplify exp
--     Bin Mult exp (Const 1.0) -> simplify exp
--     Bin Mult exp (Const 0.0) -> Const 0.0
--     Bin Mult (Const 0.0) exp -> Const 0.0
--     Bin Mult exp1 exp2 -> Bin Mult (simplify exp1) (simplify exp2)
--     -- Div - *
--     Bin Div exp (Const 1.0) -> simplify exp
--     Bin Div (Const 0.0) (Const a) -> Const 0.0
--     Bin Div exp1 exp2 -> Bin Div (simplify exp1) (simplify exp2)
--     -- Sqrt - *
--     Uno Sqrt (Const 0.0) -> Const 0.0
--     Uno Sqrt (Const 1.0) -> Const 1.0
--     Uno Sqrt exp -> Uno Sqrt (simplify exp)
