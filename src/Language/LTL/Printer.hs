{-# Language OverloadedStrings #-}
{-|
Module      : Language.LTL.Printer
Description : Printer of LTL formulae 
Copyright   : Clara Waldmann, 2016

A printer of LTL formulae into spot's and spin's format.
-}
module Language.LTL.Printer where

import Language.LTL.Data
import qualified Data.Text as T
import Data.Monoid

-- | printer of LTL formulae in spot's and spin's format
--
-- >>> toText $ (Binary U (AP "p") (Unary G (AP "q")))
-- "(p U (G q))"
--
-- >>> toText $ SpinFormula  (Binary U (AP "p") (Unary G (AP "q")))
-- "(p U ([] q))"
class ToText a where toText :: a -> T.Text
      
-- printing of the operators in spot's format
      
instance ToText Bool where
    toText = T.pack . show . fromEnum
    
instance ToText UnOp where
    toText u = 
        case u of
             Neg -> "!"
             _ -> T.pack $ show u
             
instance ToText BinOp where
    toText b = case b of
                    Or -> "|"
                    And -> "&"
                    U -> "U"
             
instance ToText Formula where
    toText f = case f of
                    AP t -> t
                    Nullary b -> toText b
                    Unary op f -> "(" <> toText op <> " " <> toText f <> ")"
                    Binary op l r ->
                        "(" <> toText l <> " " <> toText op <> " " <>
                        toText r <> ")"

                        
-- printing of the operators in spin's format            

instance ToText SpinNullary where
    toText (SpinNullary b) =
            case b of
                True -> "true"
                False -> "false"
                
instance ToText SpinUnOp where
    toText (SpinUnOp u) = 
        case u of
             Neg -> "!"
             F -> "<>"
             G -> "[]"
             X -> "X"

instance ToText SpinBinOp where
    toText (SpinBinOp b) = 
        case b of
            Or -> "||"
            And -> "&&"
            U -> "U"

instance ToText SpinFormula where
    toText (SpinFormula f) =
        case f of
            AP t -> t
            Nullary b -> toText $ SpinNullary b
            Unary op f -> "(" <> toText ( SpinUnOp op) <> " " <> toText (SpinFormula f) <> ")"
            Binary op l r ->
                        "(" <> toText (SpinFormula l) <> " " <> toText (SpinBinOp op) <> " " <>
                        toText (SpinFormula r) <> ")"
