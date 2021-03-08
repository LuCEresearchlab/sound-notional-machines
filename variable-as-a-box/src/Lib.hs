module Lib where

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

data Box = Box { boxLabel :: String
               , contents :: [Postit] } deriving (Show, Eq)
data Postit = Postit Int deriving (Show, Eq)

data Variable = Variable { name :: String
                         , value  :: Value } deriving Show
type Value = Int

type A' = Variable
type B' = Value

type A  = Box
type B  = Postit

---- Functions -------

f' :: Variable -> Value
f' = value

alpha_A :: Variable -> Box
alpha_A v = Box (name v) [(Postit (value v))]

f :: Box -> Postit
f = head . contents

alpha_B :: Value -> Postit
alpha_B = Postit


updateBox :: Int -> Box -> Box
updateBox val (Box label contents) = Box label ((Postit val):contents)

updateVariable :: Int -> Variable -> Variable
updateVariable val (Variable name value) = Variable name val

-----------------

-- Commutation proof:
-- alpha_B . f' == f . alpha_A
--
-- alpha_B . f'    == f        . alpha_A
-- Postit  . value == contents . \v -> (Box (name v) (Postit (value v)))           -- apply the definition of each function
-- Postit  . value == \x -> contents ((\v -> (Box (name v) (Postit (value v)))) x) -- apply the definition of function composition
-- Postit  . value == \x -> contents (Box (name x) (Postit (value x)))             -- apply the function "\v -> ..." (substitution)
-- Postit  . value == \x -> Postit (value x)                                       -- apply the definition of contents
-- Postit  . value == Postit . value                                               -- apply the definition of function composition backward


-- type-checked intermediate steps:
alpha_B__f' :: A' -> B
-- alpha_B__f' = alpha_B . f'
alpha_B__f' = Postit . value

f__alpha_A :: A' -> B
-- f__alpha_A = f . alpha_A
-- f__alpha_A = contents . \v -> (Box (name v) (Postit (value v)))
-- f__alpha_A = \x -> contents ((\v -> (Box (name v) (Postit (value v)))) x)
-- f__alpha_A = \x -> contents (Box (name x) (Postit (value x)))
-- f__alpha_A = \x -> Postit (value x)
f__alpha_A = Postit . value
