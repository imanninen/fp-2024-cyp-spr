import Test.Tasty (defaultMain, testGroup, TestTree)
import Parser (runParser, parseExpression)
import Test.Tasty.HUnit ((@?=), testCase, assertBool, assertFailure)
import Expr (Expr(..), BinOpeation(..), UnoOperation(..))
import Error (Error(..))


testParser :: TestTree
testParser = 
    testGroup "ParserTests" [baseDefTests, simpleTests, trikyTests]
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
        testCase "sqrt sqrt == Error" $ runParser parseExpression "sqrt sqrt" @?= Left (ParserErr "sqrt is not an ident!")
      , testCase "sqrt == Error" $ runParser parseExpression "sqrt" @?= Left (ParserErr "sqrt is not an ident!")
      , testCase "-123 == Error" $ runParser parseExpression "-123" @?= Left (ParserErr "predicate doesn't hold")

    ]
    }

main :: IO ()
main = defaultMain $ testGroup "Tests" [testParser]