module MyExpr.PrintUtils where
import Text.Printf (printf)
import MyExpr.Expr (Expr(..), BinOpeation(..), UnoOperation(..) )

printBinOp :: BinOpeation -> String
printBinOp Plus = "+"
printBinOp Minus = "-"
printBinOp Mult = "*"
printBinOp Div = "/"
printBinOp Pow = "^"

printUnOp :: UnoOperation -> String
printUnOp Sqrt = "sqrt"

printPrefix :: Expr Int -> String
printPrefix (Bin op l r) = printf "%s %s %s" (printBinOp op) (printPrefix l) (printPrefix r)
printPrefix (Uno op exp) = printf "%s %s" (printUnOp op) (printPrefix exp)
printPrefix (Const n) = show n
printPrefix (Var x) = x
