{-|
Module      : Language.LTL.Ops
Description : Manipulating LTL formulae 
Copyright   : Clara Waldmann, 2016

Manipulating LTL formulae.
-}

module Language.LTL.Ops where

import Language.LTL.Data

-- ex_f = Binary U (AP "p") (Unary G (AP "q"))

-- | decompose formula into subformulae
children :: Formula -> [(Formula -> Formula ,Formula)]
children f = 
    case f of
         AP {} -> []
         Nullary {} -> []
         Unary unop f -> [(Unary unop, f)]
         Binary binop l r -> [(\f -> Binary binop f r,l)
                             , (\f -> Binary binop l f,r)]

-- | compute positions in the syntax tree of a formula
--
-- >>> pos $ Binary U (AP "p") (Unary G (AP "q"))
-- [[],[0],[1],[1,0]]
pos :: Formula -> [Pos]
pos f = [] : do
    (i,(_,g)) <- zip [0..] $ children f 
    (i:) <$> pos g

-- | compute positions and subformulae at these positions
--
-- >>> possubs $ Binary U (AP "p") (Unary G (AP "q"))
--[ ([],Binary U (AP "p") (Unary G (AP "q")))
--, ([0],AP "p")
--, ([1],Unary G (AP "q"))
--, ([1,0],AP "q")
--]
possubs :: Formula -> [(Pos, Formula)]
possubs f = ([],f) : do
    (i,(_,g)) <- zip [0..] $ children f
    (js,h) <- possubs g
    return $ (i:js, h)
    
-- | postions where an atomic proposition is in a formula
--
-- >>> appos $ Binary U (AP "p") (Unary G (AP "q"))
-- [[0],[1,0]]
appos :: Formula -> [Pos]
appos f = do
    (ps, AP {}) <- possubs f
    return ps
    
-- | compute subformula at position
--
-- >>> peek (Binary U (AP "p") (Unary G (AP "q"))) [1]
-- Unary G (AP "q")
peek :: Formula -> Pos -> Formula
peek f pos = 
    case pos of
         [] -> f
         p:os -> peek (snd $ (children f) !! p) os
         
-- | replace the subformula at the position with another formula
--
-- >>> poke (Binary U (AP "p") (Unary G (AP "q"))) ([1],AP "p")
-- Binary U (AP "p") (AP "p")
poke :: Formula -> (Pos,Formula) -> Formula
poke f (pos,new) = 
    case pos of
         [] -> new
         p:os -> let (con, g) = (children f) !! p
                 in con $ poke g (os, new)
                 
-- | replace multiple subformulae
-- 
-- > pokes = foldl poke
pokes :: Formula -> [(Pos, Formula)] -> Formula
pokes = foldl poke
