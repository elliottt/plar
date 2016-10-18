{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}

module Prop.Base where

import Formula
import PP

import           Control.Monad ( guard )
import qualified Data.Foldable as F


type Atom = String

type Prop = Formula String


ppProp :: Prop -> Doc
ppProp  = pp

(|->) :: Atom -> Bool -> Valuation
(v |-> b) x | v == x    = b
            | otherwise = error "partial function"

(|=>) :: Atom -> Bool -> Valuation -> Valuation
(v |=> b) f x | x == v    = b
              | otherwise = f x

type Valuation = Atom -> Bool


-- | Extend a valuation with all possible assignments for a set of variables.
allValuations :: [Atom] -> Valuation -> [Valuation]
allValuations as v = go as
  where
  go (a:rest) = do t  <- [True,False]
                   v' <- go rest
                   return $ \ x -> if x == a
                                      then t
                                      else v' x
  go []       = return v

-- | Evaluate a proposition with respect to a valuation of its atoms.
eval :: Prop -> Valuation -> Bool
eval p v = go (fmap v p)
  where
  go Top       = True
  go Bot       = False
  go (Atom b)  = b
  go (Not a)   = not (go a)
  go (And a b) = go a && go b
  go (Or a b)  = go a || go b
  go (Imp a b) = not (go a) || go b
  go (Iff a b) = go a == go b
  go Forall{}  = error "forall not supported"
  go Exists{}  = error "exists not supported"

-- | Print the truth table of a proposition.
printTruthTable :: Prop -> IO ()
printTruthTable p =
  do print header
     mapM_ (print . line) (allValuations vars (const False))
  where
  vars = F.toList (atoms p)
  lens = [ 1 + max 6 (length x) | x <- vars ]

  header = hsep [ text x $$ nest len pipe | x <- vars | len <- lens ]
       <+> pp p

  line v = hsep [ pp (v x) $$ nest len pipe | x <- vars | len <- lens ]
       <+> pp (eval p v)

tautology, unsatisfiable, satisfiable :: Prop -> Bool
tautology p =
  and [ eval p v | v <- allValuations (F.toList (atoms p)) (const False) ]

unsatisfiable p = tautology (neg p)

satisfiable p = not (unsatisfiable p)


simplify1 :: Prop -> Prop

simplify1 (Not Top) = Bot
simplify1 (Not Bot) = Top
simplify1 (Not p)   = Not p

simplify1 (Or Top _) = Top
simplify1 (Or _ Top) = Top
simplify1 (Or Bot b) = b
simplify1 (Or a Bot) = a

simplify1 (And Top b) = b
simplify1 (And a Top) = a
simplify1 (And Bot _) = Bot
simplify1 (And _ Bot) = Bot

simplify1 (Imp Bot _) = Top
simplify1 (Imp _ Top) = Top
simplify1 (Imp Top p) = p

simplify1 (Iff Top b) = b
simplify1 (Iff a Top) = a
simplify1 (Iff Bot b) = Not b
simplify1 (Iff a Bot) = Not a

simplify1 p = p

simplify :: Prop -> Prop
simplify (Not p)   = simplify1 (simplify p)
simplify (And a b) = simplify1 (And (simplify a) (simplify b))
simplify (Or a b)  = simplify1 (Or (simplify a) (simplify b))
simplify (Imp a b) = simplify1 (Imp (simplify a) (simplify b))
simplify (Iff a b) = simplify1 (Iff (simplify a) (simplify b))
simplify p         = p


negative, positive :: Prop -> Bool

negative (Not _) = True
negative _       = False

positive = not . negative


negate :: Prop -> Prop
negate (Not p) = p
negate p       = Not p


-- Negation Normal Form --------------------------------------------------------

-- | Convert to negation-normal-form
nnf :: Prop -> Prop
nnf (And a b)       = nnf a       /\ nnf b
nnf (Or  a b)       = nnf a       \/ nnf b
nnf (Imp a b)       = nnf (neg a) \/ nnf b
nnf (Iff a b)       = (nnf a /\ nnf b) \/ (nnf (neg a) /\ nnf (neg b))
nnf (Not (Not p))   = nnf p
nnf (Not (And a b)) = nnf (neg a) \/ nnf (neg b)
nnf (Not (Or  a b)) = nnf (neg a) /\ nnf (neg b)
nnf (Not (Imp a b)) = nnf a /\ nnf (neg b)
nnf (Not (Iff a b)) = nnf a /\ nnf (neg b) \/ nnf (neg a) /\ nnf b
nnf p               = p

-- | The meaning of a proposition is preserved under negation normal form.
thm_nnfPreserve :: Prop -> Bool
thm_nnfPreserve p = tautology (p <=> nnf p)


nenf :: Prop -> Prop
nenf (Not (Not p))   = nenf      p
nenf (Not (And p q)) = nenf (neg p) \/ nenf (neg q)
nenf (Not (Or p q))  = nenf (neg p) /\ nenf (neg q)
nenf (Not (Imp p q)) = nenf      p  /\ nenf (neg q)
nenf (Not (Iff p q)) = nenf      p <=> nenf (neg q)
nenf (And p q)       = nenf      p  /\ nenf      q
nenf (Or p q)        = nenf      p  \/ nenf      q
nenf (Imp p q)       = nenf (neg p) \/ nenf      q
nenf (Iff p q)       = nenf      p <=> nenf      q
nenf p               = p


-- Disjunctive Normal Form -----------------------------------------------------

mkLits :: [Prop] -> Valuation -> Prop
mkLits ps v = conj [ if eval p v then p else neg p | p <- ps ]

-- | All valuations that the given formula satisfies.
allSatValuations :: Prop -> [Valuation]
allSatValuations p =
  do v <- allValuations (atoms p) (const False)
     guard (eval p v)
     return v

dnf :: Prop -> Prop
dnf p = disj [ mkLits vars v | v <- satvals ]
  where
  pvs     = atoms p
  vars    = map Atom pvs
  satvals = allSatValuations p

thm_dnfPreserve :: Prop -> Bool
thm_dnfPreserve p = tautology (p <=> dnf p)


-- Conjunctive Normal Form -----------------------------------------------------
