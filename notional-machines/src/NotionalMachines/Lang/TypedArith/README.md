# NotionalMachines.Lang.TypedArith

This is the TypedArith language.
It is basically the same as the UntypedArith language,
except that it can type its terms.
You can run its REPL via stack as follows:

```sh
stack repl
> NotionalMachines.Lang.TypedArith.Main.repl
Welcome!
TypedArith> 
```

Use the REPL to parse a TypedArith term
and to evaluate it:

```sh
TypedArith> if iszero succ 0 then false else true
true: Bool
```

Use the REPL to show the trace of the evaluation:

```sh
TypedArith> :trace if iszero succ 0 then false else true
[ If
    ( IsZero ( Succ Zero ) ) Fls Tru
, If Fls Fls Tru
, Tru
]
```

Use the REPL to determine the type of a TypedArith term:

```sh
TypedArith> :type if iszero succ 0 then false else true
if iszero succ 0 then false else true : Bool
```

The parser is specified in `Main.hs` (see `parse = ...`),
which delegates to the parser of UntypedArith.

The abstract syntax is specified in UntypedArtih's `Main.hs`
(see `data Term = ...`).

The evaluation rules are specified UntypedArith's `Main.hs`
(see `step = ...`).

The typing rules are specified in `Main.hs` (see `typeof = ...`).
