# NotionalMachines.Machine.ExpressionTree

This is the ExpressionTree notional machine.
It represents untyped lambda calculus terms as trees.

## Abstract Syntax

The abstract syntax is specified in `Main.hs`
(see `data ExpAsTree = ...`).

Here is an example using the identity function:

```sh
stack repl
> import NotionalMachines.Machine.ExpressionTree.Main
> LambdaBox "x" (Box "x")
LambdaBox "x" (Box "x")
```

## Stepping (the f)

The abstract program state transition function
in the Simulation diagram performs a step in the notional machine.
This notional machine does not define that function.
Instead, `LangInMachine/UntypedLambdaExpressionTree.hs`
defines it by mapping to untyped lambda calculus, stepping there,
and mapping back to this notional machine.
