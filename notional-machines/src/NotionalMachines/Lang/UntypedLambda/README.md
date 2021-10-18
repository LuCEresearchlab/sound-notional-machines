# NotionalMachines.Lang.UntypedLambda

This is the UntypedLambda language.
You can run its REPL via stack as follows:

```sh
stack repl
> NotionalMachines.Lang.UntypedLambda.Main.repl
Welcome!
Lambda> 
```

Use the REPL to parse an UntypedLambda expression
and to evaluate it:

```sh
Lambda> (\x.x) y
y
```

Use the REPL to show the trace of the evaluation:

```sh
Lambda> :trace (\x.x) y
[ App
    ( Lambda "x"
        ( Var "x" )
    )
    ( Var "y" )
, Var "y"
]
```

The parser is specified in `Main.hs` (see `parse = ...`).

The abstract syntax is specified in `Main.hs` (see `data Exp = ...`).

The evaluation rules are specified in `Main.hs` (see `step ... = ...`).
