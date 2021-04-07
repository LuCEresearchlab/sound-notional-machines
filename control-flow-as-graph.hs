module ControlFlowAsGraph where

--------------------
-- Bisimulation
--------------------
--
--    
--    A  --f-->  B
--
--    ^          ^
--    |          |
--  alpha_A    alpha_B
--    |          |
--    |          |
--
--    A' --f'--> B'
--
--       A  - Abstract representation (E.g., abstract data structure: List)      == Notional machine
--       A' - Concrete representation (E.g., concrete data structure: ArrayList) == Programming language
--       f  - Abstract program  state transition function                        == Notional machine "process"
--       f' - Concrete program state transition function (e.g. reduction)
--  alpha_X - Abstraction function
--
-- The abstraction is correct if:
-- alpha_B . f' == f . alpha_A
--------------------

{- 
What to model?
1. Just the syntactic structure? (pointless...)
2. Just static properties (e.g., all possible paths) without modeling execution?
3. The full burrito including modeling execution?

How to model variant 2?
* Represent B and B' as "all possible execution paths"
* To represent "all possible execution paths", use a RegEx (or, better, a DFA)
* Represent f as a function from CFG to DFA representing all possible paths
* Represent f' as a function from MethodBody to DFA representing all possible paths

RegEx or DFA?
* RegEx are a representation that's very close to structured source code
  and they neatly model the set of possible execution paths/traces
  * Sequence of statements: sequence of RegEx
  * Selection of statements: selection of RegEx
  * Repetition of statements: repetition of RegEx
* However:
  * deciding whether two RegEx are equivalent is hard
  * canonicalizing a RegEx is hard
* DFAs are a representation that's very close to the CFG
  * Map 1:1 from CFG node to DFA state
  * Each DFA state receives a unique ID
  * CFG entry node becomes DFA start state
  * CFG exit node becomes DFA final state
  * DFA alphabet is set of all unique IDs
  * Map 1:1 from CFG edges to DFA transitions
  * Label each DFA transition with ID of target state
* DFAs can be efficiently minimized into a canonical form (and thus tested for equivalence)

How to convert a MethodBody to a RegEx?
(we may want to produce a DFA instead)
* Each Sequence is a RegEx sequence "ab"
* Each Conditional is a RegEx alternative "a|b"
* Each Loop is a RegEx Kleene star "a*"

How to convert a CFG to a RegEx?
(we may want to produce a DFA instead)
* Loop nest tree (dominator analysis) or algorithm Dima used in Essence paper

How to check alpha_B . f' == f . alpha_A?
* either change our goal to show they are *equivalent*:
  * alpha_B . f' ==equivalent== f . alpha_A
  * Proof Pearl: https://www21.in.tum.de/~krauss/papers/rexp.pdf
* or ensure that both, alpha_B . f' and f . alpha_A,
  produce a canonical form of the RegEx, and then keep our goal of checking equality
  * i.e., f' and f could convert their RegEx to its unique minimal DFA
    or to a specific member (e.g., the "minimal xor automaton")
    of the family of "canonical" NFAs
    https://link.springer.com/chapter/10.1007/978-3-662-44124-4_11 
    (but that seems complex and costly)
  * see also here, where they similarly create RegExes to represent all possible CFG paths
    and try to minimize them:
    https://cstheory.stackexchange.com/questions/12361/minimizing-size-of-regular-expression
  * this somewhat chaotic write-up seems relevant as well:
    http://www.gpcet.ac.in/wp-content/uploads/2017/03/UNIT-4-2-files-merged.pdf
  * Dima's ECOOP'11 paper on essence (incl. loop identification in irreducible graphs)
    http://sape.inf.usi.ch/publications/ecoop11
  * Another paper using regex on CFG paths, for visualization!
    https://www.eecis.udel.edu/~cavazos/cisc850-spring2017/papers/Lightweight-Structured-Visualization.pdf
    > —regVIS is a tool for viewing directed graphs with
    > start and end nodes. It applies a new visualization technique,
    > which uses regular expressions as a meta-representation of all the
    > paths in an input graph; the result is a containment-based and
    > structured visualization of that graph. The tool can be configured
    > to derive these regular expressions from the input graph using
    > either the Brzozowski algebraic method or the transitive closure
    > method."
    \cite{toprakLightweightStructuredVisualization2014}
    https://www.tuhh.de/sts/research/programming-languages-program-reconstruction/regvis.html
  * or maybe the RegEx representing execution paths actually
    are "deterministic RegEx", which can directly be translated into DFAs?
    https://docs.racket-lang.org/rex/

Questions to ask, i.e., (static) properties / f / f' to check:
* Is the incPart executed at the end of a loop?
  * trivial in PL -- true
  * in NM: check in CFG
* Is there a loop?
  * in PL: check for presence of ForLoop or WhileLoop
  * in NM: check for cycle in CFG
-}

---- Data Types ------

-- NM
data ControlFlowGraph = ControlFlowGraph {
    entry :: Node
  , exit :: Node
  , nodes :: [Node]
  , edges :: [Edge] }
data Node = RectangleNode {

} | DiamondNode {

}
data Edge = Edge {
    fromNode :: Node
  , toNode :: Node }

-- PL
data MethodBody = MethodBody {
    statements :: [Statement]
}
data Statement = Statement {
    to:: Statement
} | Conditional {
    cond:: Expression
  , ifthen:: Statement
  , ifelse:: Statement
} | WhileLoop {
    cond:: Expression
  , body:: Statement
  , after:: Statement
} | ForLoop {
    cond:: Expression
  , initPart :: Statement
  , incPart :: Statement
  , body:: Statement
  , after:: Statement
} | ReturnStatemet {
}

data Expression = 

-- We need an abstract machine!
-- More specifically, given that we model control-flow,
-- we need a litte imperative language! 
-- Should we model an *assembly language* or a *structured language*?
-- if a structured language:
-- * WHILE-Programm (https://de.wikipedia.org/wiki/WHILE-Programm)
-- * IMP (Rosu/K-framework?) https://core.ac.uk/download/pdf/4823792.pdf (section 3.1 for IMP semantics)
-- * While language (Nielson, Nielson, Hanking: Principles of Program Analysis)
-- if an assembly language: (https://en.wikipedia.org/wiki/Register_machine)
-- * GOTO-Programm (https://de.wikipedia.org/wiki/GOTO-Programm)
-- * counter machine (https://en.wikipedia.org/wiki/Counter_machine)
-- * pointer machine (https://en.wikipedia.org/wiki/Pointer_machine)
-- * random access machine: RAM (https://en.wikipedia.org/wiki/Random-access_machine)
-- * random access stored program machine: RASP (https://en.wikipedia.org/wiki/Random-access_stored-program_machine)


type A' = MethodBody
type B' = MethodBody

type A  = ControlFlowGraph
type B  = ControlFlowGraph

---- Functions -------

f' :: MethodBody -> MethodBody
-- f' = value
-- f' var = case var of 
--   Variable _ value -> value


alpha_A :: MethodBody -> ControlFlowGraph
--alpha_A v = ...

f :: ControlFlowGraph -> ControlFlowGraph
--f = ...

alpha_B :: MethodBody -> ControlFlowGraph
--alpha_B = alpha_A

-----------------

-- Commutation proof:
-- alpha_B . f' == f . alpha_A
--


-- type-checked intermediate steps:
alpha_B__f' :: A' -> B
--alpha_B__f' = alpha_B . f'

f__alpha_A :: A' -> B
--f__alpha_A = f . alpha_A
