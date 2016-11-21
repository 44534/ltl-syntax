{-|
Module      : Language.LTL.Property
Description : some properties of LTL formulae 
Copyright   : Clara Waldmann, 2016

-}

module Language.LTL.Property where

import Language.LTL.Data

-- | the size of a formula
-- 
-- >>> size $ Binary U (AP "p") (Unary G (AP "q"))
-- 4
size :: Formula -> Int
size f = 
    case f of
        AP _ -> 1
        Nullary _ -> 1
        Unary _ f -> 1 + size f
        Binary _ f1 f2 -> 1 + size f1 + size f2

-- | is the first operator not a negation ?
-- 
-- >>> nonnegative $ Binary U (AP "p") (Unary G (AP "q"))
-- True
--
-- >>> nonnegative $ Unary Neg $ Binary U (AP "p") (Unary G (AP "q"))
-- False
nonnegative :: Formula -> Bool
nonnegative f = 
    case f of
         Unary Neg _ -> False
         _ -> True
