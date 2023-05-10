{-# OPTIONS_GHC -Wall -Wno-orphans #-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module NotionalMachines.LangInMachine.UntypedLambdaReduct where

import NotionalMachines.Lang.UntypedLambda.Main (Exp (..))
import NotionalMachines.Machine.Reduct.Main     (ReductExp, ReductExpF (..), updateUids)

import NotionalMachines.Meta.Bisimulation (Bisimulation, mkInjBisim, mkStepInjNM)
import NotionalMachines.Meta.Injective    (Injective, fromNM)
import NotionalMachines.Meta.LangToNM     (LangToNM (..))
import NotionalMachines.Meta.Steppable    (SteppableM, step, stepM)


--------------------
-- Lang to NM and back
--------------------
nmToLang :: ReductExp -> Maybe Exp
nmToLang (HolePlug n1 n2  _) = App <$> (nmToLang =<< n1) <*> (nmToLang =<< n2)
nmToLang (HolePipe name n _) = Lambda name <$> (nmToLang =<< n)
nmToLang (Pipe name       _) = Just (Var name)

langToNm :: Exp -> ReductExp
langToNm p = updateUids 0 (go p 0)
  where go (App e1 e2)     = HolePlug (Just (langToNm e1)) (Just (langToNm e2))
        go (Lambda name e) = HolePipe name (Just (langToNm e))
        go (Var name)      = Pipe name

------------------

--    A  --f-->  B
--
--    ^          ^
--    |          |
--  alphaA    alphaB
--    |          |
--    |          |
--
--    A' --f'--> B'

instance LangToNM Exp ReductExp where
  toNM   = langToNm

instance Injective Exp ReductExp Maybe where
  fromNM = nmToLang

instance SteppableM ReductExp Maybe where
  stepM = mkStepInjNM (step :: Exp -> Exp)

bisim :: Bisimulation Exp Exp ReductExp (Maybe ReductExp)
bisim = mkInjBisim step
-- bisim = Bisim { fLang  = eval
--               , fNM    = evalM
--               , alphaA = toNM
--               , alphaB = return . toNM }


{-

[explanation of bisimulation]...

The abstraction functions (alpha) and the functions that operate on Notional Machines (f) can be either an action performed by the student or an action performed by the notional machine. Examples?


Reduct is an educational game that aims to "teach novices core programming concepts which include functions, Booleans, equality, conditionals, and mapping functions over sets".
It "uses the rules of small-step operational semantics to provide the basic units of gameplay".
The game has a sequence of level which "progressively introduces reduction rules for a subset of JavaScript ES2015".

The language effectively implemented in Reduct departures from the syntax and semantics of JavaScript in a few ways.
For example, the author describe how `x x` does not signify application, but a collection of `x`s.
So, "in Reduct, unlike lambda calculus, the expression `(x) => x x` signifies a function that outputs two copies of its input."
That could be just a difference in syntax which, although argueably misleading, wouldn't itself make the notional machine an unsound simulation of JavaScript. Let's see the impact of that an other design decisions on the soundness of the Notional Machine.

To determine the soundness of this Notional Machine, let's try to construct an abstraction function from JavaScript to Reduct (`alpha`) and determine what are the operations that transform Reduct diagrams (`f`s).
The nodes of the diagram have stages of concreteness (A1, A2, A3). In each level, the stage of concreteness doesn't change so `f :: A_n -> A_n`, and it should be possible to map a subset of JavaScript to the diagrams of any level so `alpha_n :: A' -> A_n`.
Let's first take lambda abstraction and application.

A lambda abstraction can be directly mappeded to a node in all stages.
But in the case of application, the diagram doesn't contain a node which corresponds to the application term. To apply a lambda abstraction to a term, the student must drop a term onto a lambda abstraction. The act of dropping a term onto a lambda is therefore a part of the bisulation abstraction function (alpha) because it's an act that constructs an application term from two other terms. But by dropping a term onto a lambda the application is immediately evaluated, revealing the body of the lambda after substitution. That makes the act of dropping a term onto a lambda also part of the operation of the notional machine. Dropping a term both constructs a term (application) and runs the program (reduction). The fist issue with this is that it is not possible to construct multiple application terms before triggering substitution [give example of program that can't be constructed because of that]. The second issue is that the student is then expected to continue constructing the program after part of is was already evaluated and in fact some levels can only be solved because of that.

alpha:
- nodes given to the student
- dropping nodes into lambda abstractions


## Levels with issues:

- 7:
- 9:
- 16:
- 17: can't put lambda in hole of `star == _`


## Wrong things

1. multiple returns

1. program modification

1. application by dropping

1. the Hole and Pipe metaphor if flawded because as lambdas can contains other lambdas one must identify the holes and the corresponding pipes
-}

