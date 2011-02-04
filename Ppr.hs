-- | A pretty printer for our DataStruct

{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
-- http://www.haskell.org/haskellwiki/List_instance

module Ppr where

import Datastruct
import Text.PrettyPrint.HughesPJ

prettyprint :: Show a => ArchElem a -> String
prettyprint elem
  = (renderStyle mystyle . nest tabWidth . ppr) elem
  where mystyle = style{lineLength=80, ribbonsPerLine=1}

tabWidth = 4

-- | Pretty printing class
class Ppr a where
 ppr :: a -> Doc
 pppr :: a -> Doc
 pppr = parens . ppr

-- identity instantiation
instance Ppr Doc where
 ppr = id

instance Show a => Ppr (ArchElem a) where
  ppr (Function id name inports outport (subelems,wires) a)
    = text "Function" <+> ppr id <+> ppr name <+> vcat [ppr inports,parens (ppr outport)]
        $+$ lparen
        $+$ nest tabWidth (ppr subelems) $+$ comma
        $+$ nest tabWidth (ppr wires)
        $+$ rparen
        $+$ showA a
  ppr (Operator id op ins out a) = text "Operator" <+> ppr id <+> ppr op <+> vcat [ppr ins, pppr out] <+> showA a
  ppr (Literal id val out a) = text "Literal" <+> ppr id <+> ppr val <+> pppr out <+> showA a
  ppr (Mux id ins out sels a) = text "Mux" <+> ppr id <+> vcat [ppr ins, pppr out, ppr sels] <+> showA a
  ppr (Register id mIn out a) = text "Register" <+> ppr id <+> pppr mIn <+> pppr out <+> showA a
  ppr (PortReference p) = text "PortReference" <+> ppr p

instance Ppr String where
  ppr = text . show

instance Ppr (Maybe String) where
  ppr Nothing = text "Nothing"
  ppr (Just j) = parens (text "Just" <+> ppr j)
instance Ppr (Maybe Port) where
  ppr Nothing = text "Nothing"
  ppr (Just j) = text "Just" <+> pppr j

instance Ppr Port where
  ppr (SinglePort id) = text "SinglePort" <+> ppr id
  ppr (MultiPort id subports) = text "MultiPort" <+> ppr id <+> ppr subports

instance Ppr [Port] where
  ppr xs = brackets $ sep $ punctuate comma $ map ppr xs
instance Show a => Ppr [Wire a] where
  ppr xs = brackets $ sep $ punctuate comma $ map ppr xs
instance Show a => Ppr [ArchElem a] where
  ppr xs = brackets $ sep $ punctuate comma $ map ppr xs


instance Show a => Ppr (Wire a) where
  ppr (Wire name src dest a) = text "Wire" <+> ppr name <+> ppr src <+> ppr dest <+> showA a

showA :: Show a => a -> Doc
showA = parens . text . show

