module VariableParkingSpace where

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

data WorldState = WorldState { parkingSpace :: ParkingSpace
                             , carToPark :: Car }
data ParkingSpace = ParkingSpace { parkingSpaceType :: ParkingSpaceType
                                 , contents :: Car }
data Car = Car CarType
data CarType = Compact | SUV
data ParkingSpaceType = ParkingSpaceType CarType DriverConstraints
data DriverConstraints = Unconstrained | WomenOnly | HandicappedOnly


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
