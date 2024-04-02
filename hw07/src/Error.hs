{-# LANGUAGE InstanceSigs #-}
module Error (Error(..)) where
import Text.Printf (printf)

data Error = DivByZero [Char]
  | NegativeRoot [Char] 
  | UndefVar [Char]
  | ZeroPowZero [Char]
  | ParserErr String
  deriving Eq


instance Show Error where 
  show :: Error -> String
  show (DivByZero msg) = printf "ERROR_DIV_BY_ZERO: %s / 0" msg
  show (NegativeRoot msg) = printf "ERROR_NEGATIVE_ROOT: %s" msg
  show (UndefVar mag) = printf "ERROR_UNDEFINED_VAR: %s" mag
  show (ZeroPowZero msg) = printf "ERROR_ZERO_POW_ZERO%s" msg
  show (ParserErr msg) = printf "PARSER_ERROR_ACCURE: %s" msg