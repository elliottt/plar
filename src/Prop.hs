{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ParallelListComp #-}

module Prop where

import Formula
import PP

import qualified Data.Foldable as F
import           Data.String


type Prop = Formula String

instance IsString Prop where
  fromString = Atom


ppProp :: Prop -> Doc
ppProp  = pp

(v |-> b) x | v == x    = b
            | otherwise = error "partial function"

(v |=> b) f x | x == v    = b
              | otherwise = f x

eval :: Prop -> (String -> Bool) -> Bool
eval p v = go (fmap v p)
  where
  go Top       = True
  go Bot       = False
  go (Atom b)  = b
  go (Not a)   = not (go a)
  go (And a b) = go a && go b
  go (Or a b)  = go a || go b
  go (Imp a b) = not (go a) || go b
  go (Iff a b) = go (Imp a b /\ Imp b a)
  go Forall{}  = error "forall not supported"
  go Exists{}  = error "exists not supported"

printTruthTable :: Prop -> IO ()
printTruthTable p =
  do print header
     mapM_ (print . line) valuations
  where
  vars = F.toList (atoms p)
  lens = [ 1 + max 6 (length x) | x <- vars ]

  valuations = go vars
    where
    go [x]    = do val <- [True,False]
                   return (x |-> val)
    go (x:xs) = do val      <- [True,False]
                   versions <- go xs
                   return ((x |=> val) versions)

  header = hsep [ text x $$ nest len pipe | x <- vars | len <- lens ]
       <+> pp p

  line v = hsep [ pp (v x) $$ nest len pipe | x <- vars | len <- lens ]
       <+> pp (eval p v)
