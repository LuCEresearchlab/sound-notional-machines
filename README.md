# structured-notional-machines

Foundations for correct-by-construction notional machines.

## Paper

We work on the POPL submission in Overleaf:
https://www.overleaf.com/project/606d69ee5dd3039f53edf23e

## What's here?

* [notional-machines](https://github.com/LuCEresearchlab/structured-notional-machines/tree/main/notional-machines): we're consolidating all the notional machine formalization into this project. That's probably where you want to go.
  * `Lang/` - language implementations (the languages are small)
  * `Machine/` - notional machine implementations
  * `LangInMachine/` - bisimulation relation between language and NM
  * `Meta/` - high-level machinery to aid implementations
* [expressiontutor-backend](https://github.com/LuCEresearchlab/structured-notional-machines/tree/main/expressiontutor-backend): web backend exposing some of the functionality in `notional-machines` as micro services.
* references: other code we've been looking at.
* Other folders and files: each standalone haskell file was an attempt to write a bisimulation for a given notional machine. The other folders are stack projects trying to take specific cases. Now we want to move everything into `notional-machines` and try to make that a library to write bisimulations for language and notional machines.
  * `imperative/`
    * Other implementations for small imperative languages
  * `paper-linked-lists/`
    * Old attempt by Igor to model an NM
  * `variable-as-a-box/`
    * Old attempt by Igor to model an NM
  * `control-flow-as-graph.hs`
    * Old, incomplete attempt by Matthias to model an NM
  * `grammarAsTrain.hs`
    * Old attempt by Johan to model an NM
  * `variable-as-parking-space.hs`
    * Old, incomplete attempt by Matthias to model an NM

## Run

### Install dependencies

Install [Haskell](https://www.haskell.org/downloads/) (GHCI, stack, ...).

```sh
ghci --version
stack --version
```

### Run tests

To ensure everything is working, run the tests:

```sh
cd notional-machines
stack test --file-watch
```

### Run a language in its REPL

Run the REPL of a given language.
We currently have a sequence of five languages inspired by TAPL:

* UntypedArith
* UntypedLambda
* TypedArith
* TypedLambda
* TypedLambdaRef

For example, run the *TypedLambdaRef* REPL,
and then try some of its features:

```sh
stack repl
> NotionalMachines.Lang.TypedLambdaRef.Main.repl
LambdaRef> :help
The syntax of the language follows TAPL Ch.13
REPL commands: help, type, trace
LambdaRef> 123
123 : Nat
LambdaRef> :type 1
1 : Nat
LambdaRef> :type true
true : Bool
LambdaRef> :trace 123
LambdaRef> :trace iszero 0
LambdaRef> :trace iszero 1
LambdaRef> :trace succ 10
LambdaRef> :trace pred 10
LambdaRef> :trace pred 10
```

And now an example with refs, the entire purpose of this *TypedLambdaRef* language.

```sh
LambdaRef> (\r:Ref Nat.(\s:Ref Nat. s := 82; !r) r) (ref 13)
82 : Nat
```

In this *TypedLambdaRef* language:

* `ref 1` allocates a location to hold the value 1
* `r := 2` assigns the value 2 to the location bound by the name `r`
* `!r` dereferences the location bound by the name `r`
* `a; b` sequencing (value of first part is discarded)

```sh
LambdaRef> :trace (\r:Ref Nat.(\s:Ref Nat. s := 82; !r) r) (ref 13)
[
    ( "(\r:Ref Nat. (\s:Ref Nat. s := 82; !r) r) (ref 13)"
    , "Store: []"
    )
,
    ( "(\r:Ref Nat. (\s:Ref Nat. s := 82; !r) r) (Loc 0)"
    , "Store: [(0, 13)]"
    )
,
    ( "(\s:Ref Nat. s := 82; !(Loc 0)) (Loc 0)"
    , "Store: [(0, 13)]"
    )
,
    ( "(Loc 0) := 82; !(Loc 0)"
    , "Store: [(0, 13)]"
    )
,
    ( "unit; !(Loc 0)"
    , "Store: [(0, 82)]"
    )
,
    ( "!(Loc 0)"
    , "Store: [(0, 82)]"
    )
,
    ( "82"
    , "Store: [(0, 82)]"
    )
]
```

### Play with a notional machine

We currently have four notional machines:

* AlligatorEggs (on UntypedLambda)
* ExpressionTree (on UntypedLambda)
* ExpressionTutor (on UntypedArith, TypedArith and UntypedLambda)
* Reduct (on UntypedLambda)

For example, play with the *AlligatorEggs* notional machine.

```sh
stack repl
> import Data.Either
> import Data.Maybe
> NotionalMachines.Lang.UntypedLambda.Main.eY
Lambda "f" (App (Lambda "x" (App (Var "f") (App (Var "x") (Var "x")))) (Lambda "x" (App (Var "f") (App (Var "x") (Var "x")))))
> (toAscii . fromJust . NotionalMachines.Utils.eitherToMaybe) NotionalMachines.Lang.UntypedLambda.Main.eY
f---------------<
 x-----< x-----<
  f ---   f ---
    x x     x x
```

`eY` above is the Y-combinator.

## Run the code

Run the stack based projects (for example, `notional-machines/`)
in continous compilation/testing with

```
$ cd notional-machines
$ stack test --file-watch
```

Run the (old) haskell files in this top-level directory
using continuous compilation with

```
$ ghcid --command="ghci variable-as-parking-space.hs"
```

## What do we use this for?

* By modeling, we clarfiy our understanding of the NM:
  * What an NM really is useful for (the `f` function)
  * What the NM really is (the `A` data type)
  * How to convert a program into an NM (the `alpha` function)
  * Whether the NM is consistent (the proof)
  * Also, we understand that the NM is *specific* to a PL (`alpha`, proof)
* Once we have an executable model, we can automate assignment/assessment problems:
  * Generate (using quickcheck-style property-based testing generators)
    * Given some interesting "properties", generate PL representations (`A'`)
    * Given some interesting "properties", generate NM representations (`A`)
  * We can automatically check (automatic grading, possibly feedback generation):
    * That students drew a correct NM given a program (use `alpha`)
    * That students correctly operate inside the NM (use `f`)
* Ask students to do this formalization / implement ("programming to learn")
  * E.g., as we do in the CFG and AST labs of PF2

## Related Work / Resources

* [Bisimulation: Relating reduction systems](https://plfa.inf.ed.ac.uk/Bisimulation/)
  in the "Programming Language Foundations in Agda" online book
  by Wadler, Kokke, and Siek.
* Example of bisimulation as a tools to prove correctness of abstraction:
  M. Wang, J. Gibbons, K. Matsuda, and Z. Hu, “[Refactoring pattern matching](https://dl.acm.org/doi/10.1016/j.scico.2012.07.014),” Science of Computer Programming, vol. 78, no. 11, pp. 2216–2242, Nov. 2013
* R. Milner, “An algebraic definition of simulation between programs,” in Proceedings of the 2nd international joint conference on Artificial intelligence, San Francisco, CA, USA, Sep. 1971
* R. S. Bird, “The promotion and accumulation strategies in transformational programming,” ACM Trans. Program. Lang. Syst., vol. 6, no. 4, pp. 487–504, Oct. 1984
* C. A. Hoare, “Proof of correctness of data representations,” Acta Inf., vol. 1, no. 4, pp. 271–281, Dec. 1972
