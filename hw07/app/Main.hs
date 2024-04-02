module Main (main) where
import Expr (Expr(..), BinOpeation(..), runEval, UnoOperation(..))
import Parser (Parser(..), runParser, parseExpression, parseBinOp)

myApp :: IO ()
myApp = do
  putStrLn "Please enter the expression in presix notation to parse it!"
  line <- getLine
  case line of 
    "exit" -> putStrLn "Bye!"
    _ -> let res = runParser parseExpression line in
      case res of 
        Right (str, exp) -> do 
          print exp
          myApp
        Left err -> do 
          print err
          myApp


main :: IO ()
main = do
  putStrLn "Welcome to Igor's app!"
  myApp
