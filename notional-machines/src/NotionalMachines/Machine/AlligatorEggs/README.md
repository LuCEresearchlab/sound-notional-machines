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
> import NotionalMachines.Machine.AlligatorEggs.Main
> aid
> HungryAlligator (nameToColor "a") [Egg (nameToColor "a")]
```

## Stepping (the f)

The abstract program state transition function
in the bisimulation diagram performs a step in the notional machine:

```sh
> stepM [aid] :: Maybe [AlligatorFamilyF Color]
```

## Concrete Syntax

### Ascii Alligators

The AlligatorEggs notional machine includes
a concrete syntax representing an alligator family as ASCII art.
Here is the identity function:

```sh
> toAscii [aid]
a-<
 a
```

And here is the y-combinator:

```sh
> ay
> toAscii [ay]
g---------------<
 x-----< x-----<
  g ---   g ---
    x x     x x
```

### Image

It also includes a graphical representation.
Here is the y-combinator:

```haskell
ghci> renderDiagram "alligators.svg" 600 =<< toDiagram 1 [ay]
```

The file `alligator.svg` is created in the root folder and it's `600` wide.

## From lambda to alligator

Let's parse a lamda term, turn map it to the Alligator abstract syntax (alpha), then map to the ascii concrete representation:

```haskell
ghci> (either print (print . toAscii . ULambdaAlli.langToNm) . ULambda.parse) "\\x.x"
```

## Putting it all together

This functionality (and more) is all packaged in a REPL:

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
