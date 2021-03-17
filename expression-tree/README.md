# expression-tree

Correct-by-construction expression tree notional machine.

Run the stack based projects in continous compilation/testing:

```
$ ghcid --command="stack ghci expression-tree:lib expression-tree:test:expression-tree-test --ghci-options=-fobject-code --ghci-options=-Wall --ghci-options=-Wno-unused-top-binds" --test "main"
```

Enter ghci for a demo:

```
$ stack build; stack ghci expression-tree:test:expression-tree-test
```

then you can for example demo the Expression Tree activities with:

```
*Main Paths_expression_tree> genAndSolve generateParseActivity solveParseActivity
*Main Paths_expression_tree> genAndSolve generateUnparseActivity solveUnparseActivity
*Main Paths_expression_tree> genAndSolve generateEvalActivity solveEvalActivity
```

