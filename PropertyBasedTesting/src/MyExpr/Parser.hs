{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module MyExpr.Parser where

import Data.Char ( isAlpha, isAlphaNum, isDigit, digitToInt, isNumber, ord )
import Control.Applicative ( Alternative((<|>), empty, many) )
import MyExpr.Expr (Expr(..), BinOpeation(..), UnoOperation(..))
import MyExpr.Error (Error(..))
import GHC.Num.BigNat (bigNatPopCount)

newtype Parser a
  = Parser { runParser :: String -> Either Error (String, a)}

-- This abstraction of a parser is a Functor, which allows us to transform the parser's results. 
instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser $ \input ->
    case runParser p input of
      Left a -> Left a
      Right (suff, r) -> Right (suff, f r)

-- The parser is also an applicative functor, which simplifies composition.       
instance Applicative Parser where
  pure :: a -> Parser a
  pure res = Parser $ \str -> Right (str, res)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) f p = Parser $ \str ->
    case runParser f str of
      Right (str', f') ->
        case runParser p str' of
          Right (str'', a) -> Right (str'', f' a)
          Left err -> Left err
      Left err -> Left err

-- Monadic bind is something which expresses the idea of sequential parser application. 
-- First parse the string with this parser, and then parse the rest with that parser.  
-- This is one of two most important operations on parsers.    
instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) f p = Parser $ \str ->
    case runParser f str of
      Right (str', res) -> runParser (p res) str'
      Left err -> Left err

-- Alternative operation allows us to express that something is either this or that. 
-- Note that it favors the left-hand parser: if it succeeds consuming any prefix of the input string, 
-- the right parser will not be tried. 
instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const $ Left $ ParserErr "Empty"-- a parser which always reports an error: no strings in its language.

  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) l r = Parser $ \str ->
    case runParser l str of
      Right (str', res) -> Right (str', res)
      Left _ -> runParser r str

-- This function creates a parser which checks that a predicate holds for the first character of an input string.  
satisfy :: (Char -> Bool) -> String -> Parser Char
satisfy p errMsg = Parser $ \str ->
  case str of
    (h:t) | p h -> Right (t, h)
    _ -> Left $ ParserErr errMsg

-- Ident = Alpha AlphaNum*
-- starts with letter 
-- continues with a sequence (possibly empty) of letter or digits
parseIdent :: Parser String
parseIdent = do
    h <- satisfy isAlpha "is not an alpha"
    t <- go
    let name = h:t       
    if name `elem` keywords 
      then failParser "sqrt is not an ident!!" -- there it should fails but it not :( 
      else return (h : t)       
  where
    go = (do                    -- a sequence of symbols is a symbol followed by the sequence or an empty sequence. 
        x <- satisfy isAlphaNum "is not alpha or num" -- the first symbol is either a letter or a digit
        y <- go                 -- then goes the sequence
        return (x : y))         -- and we return the result 
      <|>
        return []               -- note that we only parse the empty sequence of letters after we've tried to match the non-empty sequence 
                                -- experiment by switching the order and see what happens then. 

-- This function creates a parser for something surrounded by parentheses. 
-- Both parentheses and the "something" are parameters. 
inParens :: Char -> Char -> Parser a -> Parser a
inParens l r p = do
  satisfy (== l) "Bad!" -- we don't bind the result of parsing here (btw, what is its type?) because we don't need it
  x <- p         -- the only thing we need is the result of parsing the "something"
  satisfy (== r) "Bad!"-- throw away this result also 
  return x       -- no parentheses made it into the result 

-- A tuple is a sequence of identifiers, separated by ',' in parentheses
-- parseIdentTuple :: Parser [String]
-- parseIdentTuple = inParens '(' ')' identSequence

-- This parser not only parses the list of identifiers, but also computes their lengths
parseIdentList :: Parser [(String, Int)]
parseIdentList = inParens '[' ']' identSequence

-- A pair is two identifiers with a comma in between, surrounded by parentheses
parsePair :: Parser (String, Int)
parsePair = inParens '(' ')' $ do
  x <- parseIdent
  satisfy (== ',') "not a ','!"
  y <- parseInt
  return (x, y)

-- This is the same parser as parsePair written in the applicative style
-- <* functions the same ways as does <*> but ignores its result on the right
parsePair' :: Parser (String, String)
parsePair' = inParens '(' ')'
  ((,) <$> parseIdent <* satisfy (== ',') "not a ','!" <*> parseIdent)

-- A sequence of identifiers separated by commas is: 
-- a single identifier followed by a sequence of comma-identifier pairs
-- or an empty sequence. 
-- Here we use the function `many :: Alternative f => f a -> f [a]` which applies its argument multiple times (until the first failure)
-- and then collects the results in the list. 
-- The function `>>` is the same as `>>=`, but it ignores its left result, only performing the effect. 
identSequence :: Parser [(String, Int)]
identSequence = (do
    h <- parsePair
    t <- many (satisfy (== ',') "not a ','!" >> parsePair)
    return (h : t)
  )
  <|>
    return []

failParser :: String -> Parser a
failParser err = Parser $ \_ -> Left $ ParserErr err

keywords :: [String]
keywords = ["sqrt"]

binOperationsList :: [Char]
binOperationsList = ['+', '-', '*', '/', '^']

parseBinOp :: Parser (Expr Int)
parseBinOp = do
  op <- satisfy binop "not a binory operation!"
  satisfy (== ' ') "not a ' '!"
  a <- parseExpression
  satisfy (== ' ') "not a ' '!"
  b <- parseExpression
  case op of
    '+' -> return $ Bin Plus a b
    '-' -> return $ Bin Minus a b
    '*' -> return $ Bin Mult a b
    '/' -> return $ Bin Div a b
    '^' -> return $ Bin Pow a b
  where binop = (`elem` binOperationsList)


helpFunc :: String -> Int
helpFunc list = read list :: Int

parseInt:: Parser Int
parseInt = do
    a <- satisfy isDigit "digit expected!"
    b <- go
    return $ helpFunc (a:b)
  where
    go = (do
      x <- satisfy isDigit "digit expected!"
      y <- go
      return (x:y)) <|>
      return []

parseConst :: Parser (Expr Int)
parseConst = do
  fmap Const parseInt

parseVar :: Parser (Expr Int)
parseVar = do
  fmap Var parseIdent

satisfySqrtKeyWord :: Parser [Char]
satisfySqrtKeyWord = Parser $ \str ->
  case str of
    s:q:r:t:l | [s, q, r, t] == "sqrt" -> Right ( l, "sqrt")
    _ -> Left $ ParserErr "sqrt expected!"

parseUnoOp :: Parser (Expr Int)
parseUnoOp = do
  satisfySqrtKeyWord
  satisfy (==' ') "is not a ' '!"
  fmap (Uno Sqrt) parseExpression


parseExpression :: Parser (Expr Int)
parseExpression = do
    parseBinOp <|> parseUnoOp <|> parseVar <|> parseConst

