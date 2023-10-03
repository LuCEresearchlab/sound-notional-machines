# sound-notional-machines

Foundations for sound-by-construction notional machines.


## What's here?

* [notional-machines](https://github.com/LuCEresearchlab/structured-notional-machines/tree/main/notional-machines): we're consolidating all the notional machine formalization into this project. That's probably where you want to go.
  * `Lang/` - language implementations (the languages are small)
  * `Machine/` - notional machine implementations
  * `LangInMachine/` - bisimulation relation between language and NM
  * `Meta/` - high-level machinery to aid implementations
* [expressiontutor-backend](https://github.com/LuCEresearchlab/structured-notional-machines/tree/main/expressiontutor-backend): web backend exposing some of the functionality in `notional-machines` as micro services.


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
REPL commands: help, type, trace, traceAlaWadler, traceAlaRacket

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


### Run the diagram playground

This is used to conveniently generate diagrams (concrete representations of notional machines) leveraging the continuous compilation feature provided by the Haskell [diagrams](https://diagrams.github.io/) library, demonstrated for example in their [quick start tutorial](https://diagrams.github.io/doc/quickstart.html).

Run it for example was:

```sh
stack exec diagram-playground-exe -- -o circle.svg -w 400 -l -s diagram-playground/Main.hs
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

## Run the code

Run the stack based projects (for example, `notional-machines/`)
in continous compilation/testing with

```
$ cd notional-machines
$ stack test --file-watch
```

Run the (old) haskell files in this top-level directory
using continuous compilation with

```
$ ghcid --command="ghci variable-as-parking-space.hs"
```

## What do we use this for?

* By modeling, we clarfiy our understanding of the NM:
  * What an NM really is useful for (the `f` function)
  * What the NM really is (the `A` data type)
  * How to convert a program into an NM (the `alpha` function)
  * Whether the NM is consistent (the proof)
  * Also, we understand that the NM is *specific* to a PL (`alpha`, proof)
* Once we have an executable model, we can automate assignment/assessment problems:
  * Generate (using quickcheck-style property-based testing generators)
    * Given some interesting "properties", generate PL representations (`A'`)
    * Given some interesting "properties", generate NM representations (`A`)
  * We can automatically check (automatic grading, possibly feedback generation):
    * That students drew a correct NM given a program (use `alpha`)
    * That students correctly operate inside the NM (use `f`)
* Ask students to do this formalization / implement ("programming to learn")
  * E.g., as we do in the CFG and AST labs of PF2

## Related Work / Resources

The two theoretical foundations we are building on are
* A formalization of programming language semantics. We're using operational semantics as described in the TAPL book. Moreover, we're using the sequences of basic language there described as basic examples of languages for which notional machines can be built and analysed.
* A formalization of program/process equivalence/correspondence. That's important because the goal is to be able to say that a notional machine (including of course its behavior) corresponds/it's equivalento to a subset of the semantics of a language. For that there are actually several approaches and we have been using a well known and established theory to here: bisimulation. On that regard, I have been reading the book "Introduction to Bisimulation and Coinduction" (by Davide Sangiorgi). It seems a good foundation to describe the relationship between some of the languages and NMs. In particular, I found useful the introductory Ch. 0 and Ch. 1, section 6.6 (The equivalence spectrum) that has a diagram showing various kinds of equivalences, Ch. 6 that talks about Simulations and it may be in some cases the appropriate terminology, Ch 4. that talks about the Weak LTS (which is the basis to define Weak Bisimulation and Simulation) that refers to systems with internal/hidden states.

Other relevant papers include:
* [Bisimulation: Relating reduction systems](https://plfa.inf.ed.ac.uk/Bisimulation/)
  in the "Programming Language Foundations in Agda" online book
  by Wadler, Kokke, and Siek.
* Example of bisimulation as a tools to prove correctness of abstraction:
  M. Wang, J. Gibbons, K. Matsuda, and Z. Hu, “[Refactoring pattern matching](https://dl.acm.org/doi/10.1016/j.scico.2012.07.014),” Science of Computer Programming, vol. 78, no. 11, pp. 2216–2242, Nov. 2013
* R. Milner, “An algebraic definition of simulation between programs,” in Proceedings of the 2nd international joint conference on Artificial intelligence, San Francisco, CA, USA, Sep. 1971
* R. S. Bird, “The promotion and accumulation strategies in transformational programming,” ACM Trans. Program. Lang. Syst., vol. 6, no. 4, pp. 487–504, Oct. 1984
* C. A. Hoare, “Proof of correctness of data representations,” Acta Inf., vol. 1, no. 4, pp. 271–281, Dec. 1972
