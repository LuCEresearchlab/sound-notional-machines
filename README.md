# structured-notional-machines

Foundations for correct-by-construction notional machines.

Run the bare bone haskell files in continuous compilation with

```
$ ghcid --command="ghci variable-as-a-box.hs"
```

Run the stack based projects in continous compilation/testing with

```
$ ghcid --command="stack ghci variable-as-a-box:lib variable-as-a-box:test:variable-as-a-box-test --ghci-options=-fobject-code" --test "main"
```
