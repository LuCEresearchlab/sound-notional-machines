# structured-notional-machines

Foundations for correct-by-construction notional machines.

## Paper

We work on the POPL submission in Overleaf:
https://www.overleaf.com/project/606d69ee5dd3039f53edf23e

## Run the code

Run the bare bone haskell files in continuous compilation with

```
$ ghcid --command="ghci variable-as-a-box.hs"
```

Run the stack based projects in continous compilation/testing with

```
$ ghcid --command="stack ghci variable-as-a-box:lib variable-as-a-box:test:variable-as-a-box-test --ghci-options=-fobject-code" --test "main"
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

* [Bisimulation: Relating reduction systems](https://plfa.inf.ed.ac.uk/Bisimulation/)
  in the "Programming Language Foundations in Agda" online book
  by Wadler, Kokke, and Siek.
* Example of bisimulation as a tools to prove correctness of abstraction:
  M. Wang, J. Gibbons, K. Matsuda, and Z. Hu, “[Refactoring pattern matching](https://dl.acm.org/doi/10.1016/j.scico.2012.07.014),” Science of Computer Programming, vol. 78, no. 11, pp. 2216–2242, Nov. 2013
* R. Milner, “An algebraic definition of simulation between programs,” in Proceedings of the 2nd international joint conference on Artificial intelligence, San Francisco, CA, USA, Sep. 1971
* R. S. Bird, “The promotion and accumulation strategies in transformational programming,” ACM Trans. Program. Lang. Syst., vol. 6, no. 4, pp. 487–504, Oct. 1984
* C. A. Hoare, “Proof of correctness of data representations,” Acta Inf., vol. 1, no. 4, pp. 271–281, Dec. 1972
