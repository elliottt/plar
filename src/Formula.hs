{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}

module Formula where

import PP

import           Data.Foldable ( Foldable, foldMap )
import           Data.List (nub)
import           Data.String ( IsString(..) )
import           Data.Traversable ( Traversable )

data Formula atom = Top
                  | Bot
                  | Atom atom
                  | Not (Formula atom)
                  | And (Formula atom) (Formula atom)
                  | Or  (Formula atom) (Formula atom)
                  | Imp (Formula atom) (Formula atom)
                  | Iff (Formula atom) (Formula atom)
                  | Forall String (Formula atom)
                  | Exists String (Formula atom)
                    deriving (Show,Functor,Foldable,Traversable)

instance IsString (Formula String) where
  fromString = Atom

instance PP atom => PP (Formula atom) where
  ppr _ Top         = text "true"
  ppr _ Bot         = text "false"
  ppr p (Atom atom) = ppr p atom
  ppr p (Not f)     = bracket (p >= 8) (char '~' <+> ppr 8 f)
  ppr p (And l r)   = bracket (p >= 7) (sep [ppr 7 l <+> text "/\\", ppr 6 r])
  ppr p (Or l r)    = bracket (p >= 6) (sep [ppr 6 l <+> text "\\/", ppr 5 r])
  ppr p (Imp l r)   = bracket (p >= 5) (sep [ppr 5 l <+> text "==>", ppr 4 r])
  ppr p (Iff l r)   = bracket (p >= 4) (sep [ppr 4 l <+> text "<=>", ppr 4 r])
  ppr _ (Forall a f)= text "forall" <+> text a <> char '.' <+> pp f
  ppr _ (Exists a f)= text "exists" <+> text a <> char '.' <+> pp f

neg :: Formula atom -> Formula atom
neg  = Not

infixr 7 /\
(/\) :: Formula atom -> Formula atom -> Formula atom
(/\)  = And

conj :: [Formula atom] -> Formula atom
conj [] = Top
conj ps = foldr1 (/\) ps

infixr 6 \/
(\/) :: Formula atom -> Formula atom -> Formula atom
(\/)  = Or

disj :: [Formula atom] -> Formula atom
disj [] = Bot
disj ps = foldr1 (\/) ps

infixr 5 ==>
(==>) :: Formula atom -> Formula atom -> Formula atom
(==>)  = Imp

infix 4 <=>
(<=>) :: Formula atom -> Formula atom -> Formula atom
(<=>)  = Iff

atoms :: Ord atom => Formula atom -> [atom]
atoms p = nub (foldMap return p)
