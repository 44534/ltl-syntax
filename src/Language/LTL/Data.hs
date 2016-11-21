{-|
Module      : Language.LTL.Data
Description : Types for LTL formulae 
Copyright   : Clara Waldmann, 2016

Abstract representation of LTL Formulae

-}
module Language.LTL.Data where

import Data.Text

-- | unary operators
data UnOp = Neg | F | G | X
    deriving (Eq, Ord, Enum, Bounded, Show)

-- | binary operators
data BinOp = Or | And | U 
    deriving (Eq, Ord, Enum, Bounded, Show)
    
{- | type for LTL formulae

abstract representation of ! (X ((F (a | b)) U (1 & (G c))) )

@
Unary Neg $
  Unary X $ 
    Binary U 
      (Unary F $ 
        Binary Or (AP "a") (AP "b") ) 
      (Binary And 
        (Nullary True) 
        (Unary G $ AP "c"))
@
-}
data Formula = AP Text
             | Nullary Bool
             | Unary UnOp Formula
             | Binary BinOp Formula Formula
    deriving (Eq, Ord, Show)
    

newtype SpinNullary = SpinNullary Bool
newtype SpinUnOp = SpinUnOp UnOp 
newtype SpinBinOp = SpinBinOp BinOp

-- | type for LTL formulae to be printed in spin's format
newtype SpinFormula = SpinFormula Formula
    deriving (Eq, Ord, Show)

-- | represents the positions in the syntax tree of a LTL formula
type Pos = [Int]
