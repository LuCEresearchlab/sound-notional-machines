# NotionalMachines.Machine.AlligatorEggs

This is the AlligatorEggs notional machine.
It represents untyped lambda calculus terms as alligator families.

## Abstract Syntax

The abstract syntax is specified in `Main.hs`
(see `data AlligatorFamilies = ...`).

Here is an example using the identity function:

```sh
stack repl
> NotionalMachines.Machine.AlligatorEggs.Main.aid
HungryAlligator a [Egg a]
> import NotionalMachines.Machine.AlligatorEggs.Main
> aid
HungryAlligator a [Egg a]
> HungryAlligator (Color "a") [Egg (Color "a")]
HungryAlligator a [Egg a]
```

## Stepping (the f)

The abstract program state transition function
in the bisimulation diagram performs a step in the notional machine:

```sh
> step [aid]
```

## Concrete Syntax

The AlligatorEggs notional machine includes
a concrete syntax representing an alligator family as ASCII art.
Here is the identity function:

```sh
> aid
HungryAlligator a [Egg a]
> toAscii [aid]
a-<
 a
```

And here is the y-combinator:

```sh
> ay
HungryAlligator g [HungryAlligator x [Egg g,OldAlligator [Egg x,Egg x]],HungryAlligator x [Egg g,OldAlligator [Egg x,Egg x]]]
> toAscii [ay]
g---------------<
 x-----< x-----<
  g ---   g ---
    x x     x x
```
