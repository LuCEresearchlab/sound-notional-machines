# notional-machines

## What's here?

* [notional-machines]:
  * `Lang/` - language implementations
  * `Machine/` - notional machine implementations
  * `LangInMachine/` - Simulation relation between language and NM
  * `Meta/` - high-level machinery to aid implementations

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

* [UntypedArith](notional-machines/src/NotionalMachines/Lang/UntypedArith/README.md)
* [UntypedLambda](notional-machines/src/NotionalMachines/Lang/UntypedLambda/README.md)
* [TypedArith](notional-machines/src/NotionalMachines/Lang/TypedArith/README.md)
* [TypedLambda](notional-machines/src/NotionalMachines/Lang/TypedLambda/README.md)
* [TypedLambdaRef](notional-machines/src/NotionalMachines/Lang/TypedLamdaRef/README.md)

For example, run the *TypedLambdaRef* REPL,
and then try some of its features:

```sh
stack repl
> NotionalMachines.Lang.TypedLambdaRef.Main.repl
LambdaRef> :help
The syntax of the language follows TAPL Ch.13
REPL commands: help, type, trace, traceAlaWadler, traceWithNameEnv
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

* [AlligatorEggs](notional-machines/src/NotionalMachines/Machine/AlligatorEggs/README.md) (on UntypedLambda)
* [ExpressionTree](notional-machines/src/NotionalMachines/Machine/ExpressionTree/README.md) (on UntypedLambda)
* ExpressionTutor (on UntypedArith, TypedArith and UntypedLambda)
* Reduct (on UntypedLambda)

For example, play with the *AlligatorEggs* notional machine.

```sh
ghci> NotionalMachines.LangInMachine.UntypedLambdaAlligatorEggs.repl "alligators.svg" 600
Welcome!
Alligator> :h
Play with the Alligator Eggs notional machine for Lambda Calculus
REPL commands: help, trace, ascii, asciiTrace, render, renderTrace

Alligator> :ascii (\l. \m. \n. l m n) (\t. \f. t) a b
l---------< t---< a b
 m-------<   f-<     
  n-----<     t      
   l m n             

Alligator> :render (\l. \m. \n. l m n) (\t. \f. t) a b
```

In the example above, the `render` and `renderTrace` commands will create an image with width `600` in the file `alligators.svg`.
