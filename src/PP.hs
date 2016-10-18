{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module PP (
    module PP
  , module Text.PrettyPrint.HughesPJ
  ) where

import Text.PrettyPrint.HughesPJ


pp :: PP a => a -> Doc
pp  = ppr 0

pretty :: PP a => a -> String
pretty  = show . pp

commas :: [Doc] -> [Doc]
commas  = punctuate comma

pipe :: Doc
pipe  = char '|'

bracket :: Bool -> Doc -> Doc
bracket useParens body | useParens = sep [ char '(', nest 2 body, char ')' ]
                       | otherwise =                 body

class PP a where
  ppr     :: Int ->  a  -> Doc
  pprList :: Int -> [a] -> Doc
  pprList _ as = brackets (fsep (commas (map pp as)))

instance PP String where
  ppr _ = text

instance PP Char where
  ppr _     = char
  pprList _ = text

instance PP Bool where
  ppr _ b | b         = text "true"
          | otherwise = text "false"
