# expressiontutor-backend

Web wrapper around [notional-machines](https://github.com/LuCEresearchlab/structured-notional-machines/tree/main/notional-machines).

## Instalation

Install Haskell, install stack, clone the repo, run this project with stack (`stack run`).

## Services

There are services for languages, with operations like the ones you can get in the REPL, and services for notional machines and their relationships with the languages.

### Languages

The languages here follow the sequence of languages from TAPL.

#### Arith

For syntax and semantics of the language see TAPL Ch.3.

* `/arith/gen`: generate an expression.
  
  Example: http://localhost:3000/arith/gen
  
* `/arith/step?e=EXPRESSION`: step an expression.

  Example, step the expression `iszero pred succ 0` http://localhost:3000/arith/step?e=iszero%20pred%20succ%200
  
* `/arith/eval?e=EXPRESSION`: evaluate an expression.

  Example, eval the expression `iszero pred succ 0` http://localhost:3000/arith/eval?e=iszero%20pred%20succ%200
  

#### Untyped lambda calculus

For syntax and semantics of the language see TAPL Ch.5.

* `/lambda/gen`: generate an expression.

  Example: http://localhost:3000/lambda/gen
  
* `/lambda/step?e=EXPRESSION`: step an expression.

  Example, step the expression `(\x.x x) (\y.y)` http://localhost:3000/lambda/step?e=(\x.x%20x)%20(\y.y)
  
* `/lambda/eval?e=EXPRESSION`: evaluate an expression.

  Example, eval the expression `(\x.x x) (\y.y)`: http://localhost:3000/lambda/eval?e=(\x.x%20x)%20(\y.y)
  

### Notional Machines

For now, only services exposing operations on Expressiontutor are available.

#### Expression Tutor

* `/et/arith?e=EXPRESSION`: Expressiontutor diagram for an expression in the Arith language.

  Example, the expression `iszero pred succ 0` http://localhost:3000/et/arith?e=iszero%20pred%20succ%200
  
* `/et/arith/step?e=EXPRESSION`: Read in the json data of an Expressiontutor diagram for an expression in the Arith language, take a step in the diagram.
  
* `/et/lambda?e=EXPRESSION`: Expressiontutor diagram for an expression in the untyped lambda calculus.

  Example, the expression `(\x.x x) (\y.y)` http://localhost:3000/et/lambda?e=(%5Cx.x%20x)%20(%5Cy.y)
  
* `/et/lambda/step?e=EXPRESSION`: Read in the json data of an Expressiontutor diagram for an expression in the untyped lambda calculus, take a step in the diagram.
  
  
  
  
