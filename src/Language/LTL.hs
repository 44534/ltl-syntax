{-|
Module      : Language.LTL
Description : library for LTL formulae 
Copyright   : Clara Waldmann, 2016

A library for LTL formulae. Contains a parser and printer to spot's and spin's format and a generator for LTL formulae that can generate random forumlae of a given size and enumerate formulae of a given size.
-}

module Language.LTL(
    -- * Abstract Representation of LTL Formulae 
    Formula(..), SpinFormula(..),
    -- * Parser and Printer
    parse, toText, ToText(..),
    -- * Properties of LTL Formulae
    module Language.LTL.Property,
    -- * LTL Generator
    forms, forms_nf , select,
    -- ** Random Formulae
    forms_uniform_nf, forms_fg_nf, forms_morefg_nf,
    -- * Patterns
    e, u, u2, r, c1, c2, q, s, f
)
where

import Language.LTL.Data
import Language.LTL.Parser
import Language.LTL.Property
import Language.LTL.Gen
import Language.LTL.Printer

