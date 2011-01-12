-- | A pretty printer for our DataStruct

{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
-- http://www.haskell.org/haskellwiki/List_instance

module Ppr where

import Datastruct
import Text.PrettyPrint.HughesPJ

tabWidth = 4

-- | Pretty printing class
class Ppr a where
 ppr :: a -> Doc

-- identity instantiation
instance Ppr Doc where
 ppr = id

instance Ppr (ArchElem a) where
  ppr (Function id name inports outport (subelems,wires) _)
    = text "Function" <+> ppr id <+> ppr name <+> vcat [ppr inports,parens (ppr outport)]
        $+$ lparen
        $+$ nest tabWidth (ppr subelems) $+$ comma
        $+$ nest tabWidth (ppr wires)
        $+$ rparen
  ppr (Operator id op ins out _) = text "Operator" <+> ppr id <+> vcat [ppr ins, parens (ppr out)]
  ppr (Literal id val out _) = text "Literal" <+> ppr id <+> ppr val <+> ppr out
  ppr (Mux id ins out sels _) = text "Mux" <+> ppr id <+> vcat [ppr ins, parens (ppr out), ppr sels]
  ppr (Register id mIn out _) = text "Register" <+> ppr id <+> ppr mIn <+> ppr out
  ppr (PortReference p) = text "PortReference" <+> ppr p

instance Ppr String where
  ppr = doubleQuotes . text

instance Ppr (Maybe String) where
  ppr Nothing = text "Nothing"
  ppr (Just j) = ppr j
instance Ppr (Maybe Port) where
  ppr Nothing = text "Nothing"
  ppr (Just j) = ppr j

instance Ppr Port where
  ppr (SinglePort id) = text "SinglePort" <+> ppr id
  ppr (MultiPort id subports) = text "MultiPort" <+> ppr id <+> ppr subports

instance Ppr [Port] where
  ppr xs = brackets $ sep $ punctuate comma $ map ppr xs
instance Ppr [Wire a] where
  ppr xs = brackets $ sep $ punctuate comma $ map ppr xs
instance Ppr [ArchElem a] where
  ppr xs = brackets $ sep $ punctuate comma $ map ppr xs


instance Ppr (Wire a) where
  ppr (Wire name src dest _) = text "wire" <+> ppr name <+> ppr src <+> ppr dest

