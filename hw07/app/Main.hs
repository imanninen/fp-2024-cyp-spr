module Main (main) where
import Expr (Expr(..), BinOpeation(..), runEval, UnoOperation(..))
import Parser (Parser(..), runParser, parseExpression, parseBinOp)

main :: IO ()
main = do
  let res = runParser parseExpression "sqrt"
  case res of 
    Left err -> print $ show err
    Right (str, exp) -> print $ show exp
