{-# Language KindSignatures, MultiParamTypeClasses, FlexibleInstances, NoMonomorphismRestriction, OverloadedStrings #-}
{-|
Module      : Language.LTL.Gen
Description : Generator for LTL formulae 
Copyright   : Clara Waldmann, 2016

A generator for LTL formulae. Random formulae of a given size can be computed and all formulae of this size can be enumerated. Some patterns are also implemented.
-}
module Language.LTL.Gen (
      -- * General Generators
      Select(..), Gen,
      gen_unop, gen_binop, times, unions,
      -- * Generators for Formulae
      -- ** Enumeration and Random Formulae
      forms_unary, forms, forms_nf, aps_gen,
      forms_uniform_nf, forms_fg_nf, forms_morefg_nf,
      -- ** Patterns
      c1, c2, e, q, r, s, u, u2, f
      ) where

import System.Random (randomRIO)
import qualified Data.Text as T 
import Data.Monoid ((<>))
import Data.List as L ((\\), nub)
import Control.Monad (replicateM)

import Language.LTL.Data
import Language.LTL.Ops


-- | typeclass for different selections of a list: pick one randomly (in IO monad), or select all (in list monad)
--
-- an instance (selector) represents a set of elements
-- 
-- >>> select [Neg, F, G, X]) :: IO UnOp
-- F
--
-- >>> (select [Neg, F, G, X]) :: [UnOp]
-- [Neg,F,G,X]
class Monad m => Select (m :: * -> *) where
    select :: [t] -> m t

-- | enumeration of all elements
instance Select [] where select = id
-- | random choice of one element
instance Select IO where 
    select xs = do
        i <- randomRIO (0,length xs - 1)
        return $ xs !! i

-- | represents a collection of sets indexed by numbers
--
-- Here a set indexed by s contains elements of size s.
type Gen m a = Int -> m a

{- | a generator for unary operators

>>> (gen_unop 2) :: IO [UnOp] 
[G,F]

>>> (gen_unop 2) :: [[UnOp]] 
[ [Neg,Neg],[Neg,F],[Neg,G],[Neg,X]
, [F,Neg],[F,F],[F,G],[F,X]
, [G,Neg],[G,F],[G,G],[G,X]
, [X,Neg],[X,F],[X,G],[X,X]
]

-}
gen_unop :: Select m => Gen m [UnOp]
gen_unop s = replicateM s $ select [minBound .. maxBound]
    
-- | a generator for binary operators
gen_binop :: Select m => Gen m [BinOp]
gen_binop s = replicateM s $ select [minBound .. maxBound]
    
{- | cross product of collections represented by generators where the sizes of the componentes are added

>>> times gen_unop gen_binop 3
([X],[U,And])
-}
times :: Select m => Gen m a -> Gen m b -> Gen m (a,b)
times genl genr s = do
        sl <- select [1..s-1]
        let sr = s - sl
        (,) <$> genl sl <*> genr sr
    
{- | unions of sets represented by selectors

>>> unions  [gen_unop 1, gen_unop 2] :: IO [UnOp ]
[G]
-}
unions :: Select m => [m a] -> m a
unions gens = do
    gen <- select gens
    gen
    
-- | select formula of given size beginning with an unary operator
-- s.th. neither F F, G G or Neg Neg occur
-- 
-- the allowed binary and unary operators and the atomic propositions are given
forms_unary :: Select m => [BinOp] -> [UnOp] -> [T.Text] -> Gen m Formula
forms_unary binops unops aps s = do 
        f <- forms binops unops aps (s-1)
        op <- select $ case f of
                            Unary unop _ | unop `elem` [F,G,Neg] -> 
                                filter (/= unop) unops
                            _ -> unops
        return $ Unary op f
                       
forms_binary :: Select m => [BinOp] -> [UnOp] -> [T.Text] -> Gen m Formula
forms_binary binops unops aps s = do
    (l,r) <- times (forms binops unops aps) (forms binops unops aps) (s-1)
    op <- select $ case r of
                        Binary binop _ _ | binop `elem` [And, Or] ->
                            filter (/= binop) binops
                        _ -> binops
    return $ Binary op l r
                       
-- | select a formula of a given size
-- 
-- the allowed binary and unary operators and the atomic propositions are given
--
-- >>> forms [And, Or, U] [F, G, X, Neg] ["p0","p1"] 3
-- Unary G (Unary F (AP "p1"))
forms :: Select m => [BinOp] -> [UnOp] -> [T.Text] -> Gen m Formula
forms binops unops aps s = 
    case s of
         s | s >= 3 -> unions $ 
                        [ forms_binary binops unops aps s 
                        , forms_unary binops unops aps s]
         2 -> unions [forms_unary binops unops aps s]
         1 -> unions [ AP <$> select aps] 

{- | generate a given number of atomic propositions
that are numbered increasingly
z_i <= 1 + max { z1,..., z_(i-1) }

>>> aps_gen 3 [] [1, 2, 3, 4]
[1,2,1]
-}
aps_gen :: (Select m, Eq a) => Int -> [a] -> [a] -> m [a]
aps_gen k old new =
    if k > 0
       then do
           bnew <- case (null old, null new) of 
                        (True, _ ) -> return True
                        (_, True) -> return False
                        _ -> select [True, False]
           if bnew
              -- take a new atomic proposition
              then do
                  xs <- aps_gen (k-1) (nub $ head new:old) (tail new)
                  return $ head new : xs
              -- take one atomic proposition that has already been used
              else do
                  x <- select (nub old)
                  xs <- aps_gen (k-1) old new
                  return $ x : xs
       else return []

{- | select a formula of a given size where the atomic propositions are
 numbered increasingly

a generator for formulae and the maximal number of atomic propositions used are given 
(if the number of atomic propositions is at least as large as the desired size we obtain all formulae of this size when enumerating)

>>> forms_nf (forms [And,Or,U] [F,G,X,Neg]) 5 15
Unary Neg 
  (Binary U 
    (Unary Neg 
      (Binary And 
        (Binary U   (Unary X (AP "p0"))   (AP "p0")) 
        (Binary U 
          (Unary G (Unary X (AP "p1"))) 
          (Unary X (AP "p2"))
        )
      )			
    ) 
    (AP "p3")
  )
-}
forms_nf :: Select m => ([T.Text] -> Gen m Formula) -> Int -> Gen m Formula
forms_nf forms ap s = do
    -- generate a formula with the only atomic proposition "p"
    f <- forms ["p"] s
    -- compute the positions in the formula where the atomic proposition is
    let poss = appos f
        nraps = length poss
    -- generate enough atomic propositions
    aps <- aps_gen nraps [] $ map (\i -> AP $ "p" <> T.pack (show i)) [0..(ap-1)]
    -- replace the occurence of "p" by the generated atomic proposition
    return $ pokes f $ zip poss aps
    
-- | select formulae where the operators are chosen uniformely
-- 
-- > forms_uniform_nf = forms_nf (forms [minBound..maxBound][minBound..maxBound])
forms_uniform_nf = forms_nf (forms [minBound..maxBound][minBound..maxBound])

-- | seleft formulae of the F,G fragment of LTL (only the temporal operators F and G are allowed)
--
-- > forms_fg_nf = forms_nf (forms ([minBound..maxBound] L.\\ [U]) [F,G,Neg])
-- Generate a random formula of size 3 with at most 2 atomic propositions:
-- 
-- >>> (forms_fg_nf 2 3) :: IO Formula
-- Unary G (Unary F (AP "p0"))
--
-- Generate all formulae of size 3 with at most 2 atomic propositions:
-- 
-- >>> (forms_fg_nf 2 3) :: [Formula]
-- [Binary Or (AP "p0") (AP "p1")
-- ,Binary Or (AP "p0") (AP "p0")
-- ,Binary And (AP "p0") (AP "p1")
-- ,Binary And (AP "p0") (AP "p0")
-- ,Unary G (Unary F (AP "p0"))
-- ,Unary Neg (Unary F (AP "p0"))
-- ,Unary F (Unary G (AP "p0"))
-- ,Unary Neg (Unary G (AP "p0"))
-- ,Unary F (Unary Neg (AP "p0"))
-- ,Unary G (Unary Neg (AP "p0"))
-- ]
forms_fg_nf = forms_nf (forms ([minBound..maxBound] L.\\ [U]) [F,G,Neg])

-- | select formulae where the operators F and G occur with higher probability
--
-- > forms_morefg_nf = forms_nf (forms [minBound..maxBound] ([minBound..maxBound]++[F,G]))
forms_morefg_nf = forms_nf (forms [minBound..maxBound] ([minBound..maxBound]++[F,G]))
    
-- Patterns

e :: Int -> Formula
e n = foldl (\f i -> Binary And f ( Unary F $ AP $ "p" <> T.pack (show i)))
    (Unary F $ AP $ "p1")
    [2..n]
u :: Int -> Formula
u n = foldl (\f i -> Binary U f (AP $ "p" <> T.pack (show i))) (AP $ "p1") [2..n]

u2 :: Int -> Formula
u2 n = foldr (\i f -> Binary U f (AP $ "p" <> T.pack (show i))) (AP $ "p" <> T.pack (show n)) [1..(n-1)]

r :: Int -> Formula
r n = foldl (\f i -> Binary And f ((Binary Or (Unary G $ Unary F $ AP $ "p" <> T.pack (show i)) (Unary F $ Unary G $ AP $ "p" <> T.pack (show (i+1)))))) 
    (Binary Or (Unary G $ Unary F $ AP "p1") (Unary F $ Unary G $ AP "p2"))
    [2..n]

c1 :: Int -> Formula
c1 n = foldl (\f i -> Binary Or f (Unary G $ Unary F $ AP $ "p" <> T.pack (show i))) (Unary G $ Unary F $ AP "p1") [2..n]

c2 :: Int -> Formula
c2 n = foldl (\f i -> Binary And f (Unary G $ Unary F $ AP $ "p" <> T.pack (show i))) (Unary G $ Unary F $ AP "p1") [2..n]

q :: Int -> Formula
q n = foldl (\f i -> Binary And f (Binary Or (Unary G $ AP $ "p" <> T.pack (show i)) (Unary F $ AP $ "p" <> T.pack (show (i+1)))))
    (Binary Or (Unary G $ AP "p1") (Unary F $ AP "p2"))
    [2..n]

s :: Int -> Formula
s n = foldl (\f i -> Binary Or f (Unary G $ AP $ "p" <> T.pack (show i))) (Unary G $ AP "p1") [2..n]

f :: Int -> Formula
f n = foldl 
    (\f i -> Binary And f (Binary Or (Unary Neg $ Unary G $ Unary F $ AP $ "p" <> T.pack (show i)) (Unary G $ Unary F $ AP $ "q" <> T.pack (show i)  )) )
    (Binary Or (Unary Neg $ Unary G $ Unary F $ AP "p1") (Unary G $ Unary F $ AP "q1")) [2..n]
