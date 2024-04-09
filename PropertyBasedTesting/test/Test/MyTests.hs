module Test.MyTests (props) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import qualified MyExpr.Expr as My
import MyExpr.Error (Error(..))
import MyExpr.PrintUtils (printPrefix)
import MyExpr.Parser (parseExpression, Parser, runParser)
import Debug.Trace (putTraceMsg, trace)

genInt :: Gen Int
genInt = Gen.int (Range.linear 1 100)

testTest :: Property
testTest = property $ do
  i <- forAll genInt
  assert (i > 0)

genBinOp :: Gen My.BinOpeation
genBinOp = Gen.element [My.Plus, My.Minus, My.Mult, My.Div, My.Pow]

genUnOp :: Gen My.UnoOperation
genUnOp = Gen.element [My.Sqrt]

genExpr :: Int -> Gen (My.Expr Int)
genExpr n =
  Gen.recursive
    Gen.choice
    [ -- non-recursive generators
      numGen
    , varGen
    ]
    [ -- recursive generators
      binOpGen
    , unOpGen 
    ]
  where
    numGen = My.Const <$> Gen.int (Range.constant 0 n)
    varGen = My.Var <$> Gen.string (Range.constant 1 n) Gen.alpha
    binOpGen = do
      op <- genBinOp
      Gen.subterm2 (genExpr n) (genExpr n) (My.Bin op)
    unOpGen = do
      op <- genUnOp
      Gen.subterm (genExpr n) (My.Uno op)
    


-- parser . printer == id
parserPrinterIsId :: MonadTest m => (My.Expr Int -> String) -> (String -> Either Error (String, My.Expr Int)) -> My.Expr Int -> m ()
parserPrinterIsId printer parser ast =
  case parser (printer ast) of
    Right (_, r) -> r === ast
    _ -> failure

prop_printerParserPrefix :: Property
prop_printerParserPrefix = property $ do
  expr <- forAll $ genExpr 100
  parserPrinterIsId printPrefix (runParser parseExpression) expr

props :: [TestTree]
props = [testProperty "Parser tests" prop_printerParserPrefix]