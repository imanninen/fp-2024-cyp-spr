import Test.Tasty (defaultMain, testGroup, TestTree)
import Parser (runParser, parseExpression)
import Test.Tasty.HUnit ((@?=), testCase, assertBool, assertFailure)
import Expr (Expr(..), BinOpeation(..), UnoOperation(..), runEval)
import Error (Error(..))


testParser :: TestTree
testParser = 
    testGroup "Parser tests" [baseDefTests, simpleTests, trikyTests]
  where
    { 
    baseDefTests = testGroup "Base def tests" [
        testCase "123 == Const 123" $ runParser parseExpression "123" @?= Right ("", Const 123)
      , testCase "xyz == Var xyz" $ runParser parseExpression "xyz" @?= Right ("", Var "xyz")
      , testCase "sqrt 123 == Uno Sqrt 123" $ runParser parseExpression "sqrt 123" @?= Right ("", Uno Sqrt (Const 123))
      , testCase "sqrt xyz == Uno Sqrt Var xyz" $ runParser parseExpression "sqrt xyz" @?= Right ("", Uno Sqrt (Var "xyz"))
      , testCase "^ 1 2 == Bin (^) 1 2" $ runParser parseExpression "^ 1 2" @?= Right ("",  Bin Pow (Const 1) (Const 2))
    ];
    simpleTests = testGroup "Simple tests" [
        testCase "+ 123 45 == Bin (+) 123 45" $ runParser parseExpression "+ 123 45" @?= Right ("", Bin Plus (Const 123) (Const 45))
      , testCase "* xyz 123 == Bin (*) xyz 123" $ runParser parseExpression "* xyz 123" @?= Right ("", Bin Mult (Var "xyz") (Const 123))
      , testCase "/ xyz sqr == Bin (/) xyz sqr" $ runParser parseExpression "/ xyz sqr" @?= Right ("", Bin Div (Var "xyz") (Var "sqr"))
      , testCase "+ 123 * 45 6 == Bin (+) 123 (Bin (*) 45 6)" $ runParser parseExpression "+ 123 * 45 6" @?= Right ("", Bin Plus (Const 123) (Bin Mult (Const 45) (Const 6)))
      , testCase "+ * 123 45 6 == Bin (+) (Bin (*) 123 45) 6" $ runParser parseExpression "+ * 123 45 6" @?= Right ("", Bin Plus (Bin Mult (Const 123) (Const 45)) (Const 6))
      , testCase "/ sqrt 123 xyz == Bin (/) (Sqrt 123) xyz" $ runParser parseExpression "/ sqrt 123 xyz" @?= Right ("", Bin Div (Uno Sqrt (Const 123)) (Var "xyz"))
    ];
    trikyTests = testGroup "Triky tests" [
        testCase "sqrt sqrt == Error" $ runParser parseExpression "sqrt sqrt" @?= Left (ParserErr "digit expected!")
      , testCase "sqrt == Error" $ runParser parseExpression "sqrt" @?= Left (ParserErr "digit expected!")
      , testCase "-123 == Error" $ runParser parseExpression "-123" @?= Left (ParserErr "digit expected!")
    ]
    }

evalEnv :: [(String, Double)]
evalEnv = [("x", 42), ("y", 39), ("two", 2)]

testEval :: TestTree
testEval = testGroup "Eval tests" [baseTests, simpleTests]
  where {
    baseTests = testGroup "Base tests" [
      testCase "Const 2 == 2" $ runEval (Const 2) evalEnv @?= Right 2
      , testCase "Var x == 42" $ runEval (Var "x") evalEnv @?= Right 42
      , testCase "Var z == Error" $ runEval (Var "z") evalEnv @?= Left (UndefVar "z")
    ];
    simpleTests = testGroup "Simple tests" [
      testCase "(two + 2) * 2 == 8" $ runEval (Bin Mult (Bin Plus (Var "two") (Const 2)) (Const 2)) evalEnv @?= Right 8
      , testCase "two * 2 == 4" $ runEval (Bin Mult (Var "two") (Const 2)) evalEnv @?= Right 4
      , testCase "two / 2 == 1" $ runEval (Bin Div (Var "two") (Const 2)) evalEnv @?= Right 1
      , testCase "two - 2 == 0" $ runEval (Bin Minus (Var "two") (Const 2)) evalEnv @?= Right 0
      , testCase "two ^ 2 == 2" $ runEval (Bin Pow (Var "two") (Const 2)) evalEnv @?= Right 4
      , testCase "two / 0 == Error" $ runEval (Bin Div (Var "two") (Const 0)) evalEnv @?= Left (DivByZero "2.0")
      , testCase "sqrt -1 == Error" $ runEval (Uno Sqrt (Bin Minus (Const 0) (Const 1))) evalEnv @?= Left (NegativeRoot "-1.0")
    ]
    }

main :: IO ()
main = defaultMain $ testGroup "Tests" [testParser, testEval]