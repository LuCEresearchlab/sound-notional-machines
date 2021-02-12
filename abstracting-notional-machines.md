# Abstracting Notional Machines

## Abstract

Notinal machines are devices that abstract away details of a programming language semantics and focus on some of its aspects for educational purposes. As an abstraction, it must be consistent with the corresponding concrete language. Unfortunately notional machines are not defined formally (CITE definitions) and are often times used inconsistently (CITE instances of that). We survey a few uses of notional machines, show these inconsistencies, and proceed to define them formally and prove they are consistent with the programming language they represent.


## Introduction

...

Notional machines as methafors...
but there's a risk the methafor is not sound...

...

experts when reasoning about programs may often not do it with a lot of rigour and not being very systematic.
That's because experts know what to focus on and know what is more or less important in the reasoning process.
But novices don't have enough experience to know what aspects is safe to brush over so it's important to be more rigrous and systematic when teaching.

This framework can guide teachers through a rigourous process that will result in a more rigourously conceived notional machine.

...


pitch:
- There are many notional machines out there... but how do you know they are correct? How can we know they are self consistent and consistent with the language aspect they aim to represent.
- Looking at notional machines as abstractions over languages, we can use techniques for proving the correctness of data abstractions as a way to guide the development of consistent and correct notional machines.
REPHRASING:
We use the formalization of abstraction consistency as a systematic way to build and verify the consistency of notional machines.
We apply this method to 3 notional machines.
- We use these techniques to find inconsistencies in instances of notional machines found in the literature and propose solution.

...

contributions:
- We adapt a well known framework used to reason about abstraction to reason about notional machines.
- We use this reasoning framework to rigously specify 3 know notional machines.
- We fully implement one of these notional machines.
- We use this reasoning framework to identify inconsistencies in previously published notional machines.

...

-----------------

```
    A  --f-->  B

    ^          ^
    |          |
  alpha_A    alpha_B
    |          |
    |          |

    A' --f'--> B'


A - Abstract representation (E.g., abstract data structure: List) == Notional machine

A' - Concrete representation (E.g., concrete data structure: ArrayList) == Programming language

f - Abstract program == Notional machine "process"?

f' - Program state transition function (e.g. reduction)

alpha_X - Abstraction Function


The abstraction is correct if:
alpha_B . f' == f . alpha_A

```
-----------------


## Notional machines

### Expression tree

#### Activities

##### Evaluation

see implementation...


##### Type checking

We want to make clear that to typecheck we don't need the runtime values so that's our chance to express this in the types.

```
A' :: (ExpText, Env)
A' = (express text, implicit/explicit env)

alpha_A :: (ExpText, Env) -> ExpTreeDiagram
alpha_A = alpha

f :: ExpTreeDiagram -> TypedExpTreeDiagram
f = 
```

We don't want `f' :: ExpText -> Type` because we want `B` to be a labled tree and `alpha_B` can't produce a tree from just a type.

```
f' :: (ExpText, Env) -> Either PosError TypingDerivation

alpha_B :: Either PosError TypingDerivation -> Either PosError TypedExpTreeDiagram
alpha_B = trivial... excentially the same tree
```


### Stack and heap diagram

TODO: take it from the Miro board


### The copies model of recursion

TODO


## Conclusion

A program that implements the formalism presented here is cable of generating notional machines from code, transforming programs and transforming notional machines in a mutually consistent way.
Proving the "promotion condition" in this program guarantees that the underlaying notional machine is a consistent abstraction.
As an additional result, if the program is augmented in a way that allows for students to input notional machine instances which correspond to a given code, one is able to automatically very the correctness of what the students produced.

