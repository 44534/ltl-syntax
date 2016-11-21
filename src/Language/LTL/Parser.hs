{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Language.LTL.Parser
Description : Parser of LTL formulae 
Copyright   : Clara Waldmann, 2016

A parser for LTL formulae in spot's format.
-}

module Language.LTL.Parser where

import Language.LTL.Data

import qualified Data.Text as T
import Data.Attoparsec.Text
import Data.Attoparsec.Expr
import Control.Applicative


-- | the parser of one LTL formula in spot's format
--
-- >>> parse "(p) U (G q)"
-- Right (Binary U (AP "p") (Unary G (AP "q")))
parse = parseOnly formula

-- | expression parser for LTL formulae
formula :: Parser Formula
formula = buildExpressionParser 
    [ [Prefix (token "!" *> return (Unary Neg)) ]
    , [Prefix (token "X" *> return (Unary X)) ]
    , [Prefix (token "F" *> return (Unary F)), Prefix (token "G" *> return (Unary G)) ]
    , [Infix (token "U" *> return (Binary U)) AssocRight ]
    , [Infix (token "&" *> return (Binary And)) AssocLeft ]
    , [Infix (token "|" *> return (Binary Or)) AssocLeft ]
    ] atomic
    
-- | parser for nullary operators, atomic propositions and formulae enclosed in parentheses
--
-- >>> parseOnly atomic "(p1)"
-- Right (AP "p1")
atomic :: Parser Formula
atomic = token "0" *> return (Nullary False)
    <|> token "1" *> return (Nullary True)
    <|> AP <$> identifier
    <|> parens formula
    
-- | parser for names of atomic propositions, removing withespace after it
--
-- >>> parseOnly identifier "p1   "
-- Right "p1"
identifier :: Parser T.Text
identifier = takeWhile1 (inClass "0-9a-zA-Z_-") <* whitespace
    
-- | parser for things enclosed in some strings
--
-- >>> parseOnly (embracedBy identifier "[" "]") "[ p1  ]"
-- Right "p1"
embracedBy :: Parser a -> T.Text -> T.Text -> Parser a
embracedBy p s1 s2 = do
  token s1
  r <- p
  token s2
  return r
    
-- | parser for formulae enclosed in parentheses
--
-- > parens p = embracedBy p "(" ")"
--
-- >>> parseOnly (parens atomic) "((p1))"
-- Right (AP "p1")
parens :: Parser a -> Parser a
parens p = embracedBy p "(" ")"
    
-- | parser for tokens, removing all withespace after it
token :: T.Text -> Parser ()
token s = string s *> whitespace

whitespace = skipSpace
