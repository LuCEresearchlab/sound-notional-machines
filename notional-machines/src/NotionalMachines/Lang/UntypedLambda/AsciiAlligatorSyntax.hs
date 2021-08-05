{-# OPTIONS_GHC -Wall -Wno-orphans #-}

{-# LANGUAGE LambdaCase #-}

module NotionalMachines.Lang.UntypedLambda.AsciiAlligatorSyntax where

import NotionalMachines.Lang.UntypedLambda.Main (Exp(..))
import NotionalMachines.Machine.AlligatorEggs.AsciiSyntax (AsAsciiAlligators, toAscii, hungryAlligator, oldAlligator, egg)

--------------------------------------------
-- Ascii Alligators representation of Exp --
--------------------------------------------
instance AsAsciiAlligators Exp where
  toAscii = \case
    Var name           -> egg name
    Lambda name e      -> hungryAlligator name (toAscii e)
    App e1 e2 @ App {} -> toAscii e1 <> oldAlligator (toAscii e2)
    App e1 e2          -> toAscii e1 <> toAscii e2


