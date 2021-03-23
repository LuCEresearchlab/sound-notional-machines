module ControlFlowAsGraph where

--------------------
-- Bisimulation
--------------------
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

---- Data Types ------

data WorldState = WorldState {
    cfg :: ControlFlowGraph
  , programCounter :: Node }
data ControlFlowGraph = ControlFlowGraph {
    entry :: Node
  , exit :: Node
  , nodes :: ??
  , edges :: ?? }


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
data ProgramState = ProgramState { variable :: Variable
                                 , valueToAssign :: Value }
data Variable = Variable { variableType :: Type
                         , value  :: Value }
data Value = Value Type Int
type Type = Int


type A' = ProgramState
type B' = ProgramState

type A  = WorldState
type B  = WorldState

---- Functions -------

f' :: ProgramState -> ProgramState
-- f' = value
-- f' var = case var of 
--   Variable _ value -> value
f' (ProgramState (Variable typ variableValue) (Value typ valueInt)) =
  ProgramState (Variable typ (Value typ valueInt)) null


alpha_A :: ProgramState -> WorldState
--alpha_A v = ...

f :: WorldState -> WorldState
--f = ...

alpha_B :: ProgramState -> WorldState
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
