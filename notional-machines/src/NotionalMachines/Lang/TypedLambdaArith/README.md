# NotionalMachines.Lang.TypedLambdaArith

This is the TypedLambdaArith language.
You can run its REPL via stack as follows:

```sh
stack repl
> NotionalMachines.Lang.TypedLambdaArith.Main.repl
Welcome!
TypedLambda> 
```

Use the REPL to parse an UntypedLambda expression
and to evaluate it:

```sh
TypedLambda> (\x:Nat.succ x) 0
succ (x) : Nat
```

**BUG?** Shouldn't this produce `succ (0)`?

Use the REPL to show the trace of the evaluation:

```sh
TypedLambda> :trace (\x:Nat.succ x) 0
[ App
    ( Lambda "x" TyNat
        ( Succ
            ( Var "x" )
        )
    ) Zero
, Succ
    ( Var "x" )
]
```

**BUG?** Shouldn't this substitute `0` for `Var "x"`?

The parser is specified in `Main.hs` (see `parse = ...`).

The abstract syntax is specified in `Main.hs` (see `data Term = ...`).

The evaluation rules are specified in `Main.hs` (see `step ... = ...`).
