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
[(λx.x) y, y]
```

Lambdas can be written with a back slash or de UTF-8 symbol.

An example with Church booleans:

```sh
Lambda> :trace (λl. λm. λn. l m n) (λt. λf. t) v w
[ (λl.(λm.(λn.l m n))) (λt.(λf.t)) v w
, (λm.(λn.(λt.(λf.t)) m n)) v w
, (λn.(λt.(λf.t)) v n) w
, (λt.(λf.t)) v w
, (λf.v) w
, v ]
```

The parser is specified in `Main.hs` (see `parse = ...`).

The abstract syntax is specified in `Main.hs` (see `data Exp = ...`).

The evaluation rules are specified in `Main.hs` (see `step ... = ...`).
