# NotionalMachines.Lang.UntypedArith

This is the UntypedArith language.
You can run its REPL via stack as follows:

```sh
stack repl
> NotionalMachines.Lang.UntypedArith.Main.repl
Welcome!
Arith> 
```

Use the REPL to parse an UntypedArith expression
and to evaluate it:

```sh
Arith> if iszero succ 0 then false else true
true
```

Use the REPL to show the trace of the evaluation:

```sh
Arith> :trace if iszero succ 0 then false else true
[ If
    ( IsZero ( Succ Zero ) ) Fls Tru
, If Fls Fls Tru
, Tru
]
```

The parser is specified in `Main.hs` (see `parse = ...`).

The abstract syntax is specified in `Main.hs` (see `data Term = ...`).

The evaluation rules are specified in `Main.hs` (see `step = ...`).
