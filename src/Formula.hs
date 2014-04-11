{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Formula where

import PP

import           Data.Foldable ( Foldable, foldMap )
import qualified Data.Set as Set
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

instance PP atom => PP (Formula atom) where
  ppr _ Top         = text "true"
  ppr _ Bot         = text "false"
  ppr p (Atom atom) = ppr p atom
  ppr _ (Not f)     = char '~' $$ ppr 10 f
  ppr _ (And l r)   = pp l <+> text "/\\" <+> pp r
  ppr _ (Or l r)    = pp l <+> text "\\/" <+> pp r
  ppr _ (Imp l r)   = pp l <+> text "==>" <+> pp r
  ppr _ (Iff l r)   = pp l <+> text "<=>" <+> pp r
  ppr _ (Forall a f)= text "forall" <+> text a <> char '.' <+> pp f
  ppr _ (Exists a f)= text "exists" <+> text a <> char '.' <+> pp f

infixl 7 /\
(/\) :: Formula atom -> Formula atom -> Formula atom
(/\)  = And

infixl 6 \/
(\/) :: Formula atom -> Formula atom -> Formula atom
(\/)  = Or

(==>) :: Formula atom -> Formula atom -> Formula atom
(==>)  = Imp

(<=>) :: Formula atom -> Formula atom -> Formula atom
(<=>)  = Imp

atoms :: Ord atom => Formula atom -> Set.Set atom
atoms  = foldMap Set.singleton
