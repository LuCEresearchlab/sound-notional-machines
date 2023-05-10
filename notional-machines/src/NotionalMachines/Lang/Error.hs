{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}

module NotionalMachines.Lang.Error where

import           Prettyprinter (Doc, Pretty, dot, pretty, squotes, (<+>))
import qualified Text.Parsec   as Parsec (ParseError)

data Error = ParseError Parsec.ParseError
           | TypeError String
           | RuntimeError String
           | InternalError String
  deriving (Eq, Show)

instance Pretty Error where
    pretty (ParseError parsecError) =    "Parse error" <+> pretty (show parsecError)
    pretty (TypeError m)            =    "Type error:" <+> pretty m
    pretty (RuntimeError m)         = "Runtime error:" <+> pretty m
    pretty (InternalError m)        = "Internal error:" <+> pretty m

instance Pretty Parsec.ParseError where
    pretty = pretty . show

typeOfEq :: (Pretty term, Pretty typ1, Pretty typ2, Eq typ1) =>
            (term -> Either Error typ1) -> term -> term -> typ1 -> typ2 -> Either Error typ2
typeOfEq rec ctx t typ typ3 = do typ1 <- rec t
                                 if typ1 == typ then return typ3
                                                else mismatch ctx typ typ1 t

mismatch :: (Pretty term, Pretty typ1, Pretty typ2) =>
            term -> typ1 -> typ2 -> term -> Either Error ty
mismatch ctxTerm expected found term = Left . TypeError . show $
       "expected" <+> q expected <+>
            "but" <+> q term     <+>
       "has type" <+> q found    <+>
  "in expression" <+> q ctxTerm   <> dot
      where q :: Pretty p => p -> Doc c
            q = squotes . pretty
