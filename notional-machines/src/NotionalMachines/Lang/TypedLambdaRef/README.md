# NotionalMachines.Lang.TypedLambdaRef

This is the TypedLambdaRef language.
You can run its REPL via stack as follows:

```sh
stack repl
> NotionalMachines.Lang.TypedLambdaRef.Main.repl
Welcome!
LambdaRef> 
```

Use the REPL to parse an TypedLambdaRef expression
and to evaluate it:

```sh
LambdaRef> (\x:Nat.succ x) 0
1 : Nat
```

Use the REPL to show the trace of the evaluation:

```sh
LambdaRef> :trace (\x:Nat.succ x) 0
[
    ( "(\x:Nat. succ x) 0"
    , "Store: []"
    )
,
    ( "1"
    , "Store: []"
    )
]
```

Use the REPL to determine the type of a TypedLambdaRef term:

```sh
LambdaRef> :type (\x:Nat.succ x) 0
(\x:Nat.succ x) 0 : Nat
```

The parser is specified in `ParserUnparser.hs` (see `parse = ...`).

The abstract syntax is specified in `AbstractSyntax.hs` (see `data Term = ...`).

The evaluation rules are specified in `AbstractSyntax.hs` (see `step' = ...`).

The typing rules are specified in `AbstractSyntax.hs` (see `typeof = ...`).
