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
alpha_A v = Box (name v) ((Postit (value v)):[])

f :: Box -> Postit
f = head . contents

alpha_B :: Value -> Postit
alpha_B = Postit


updateBox :: Int -> Box -> Box
updateBox val box = Box (boxLabel box) ((Postit val):(contents box))

updateVariable :: Int -> Variable -> Variable
updateVariable val var = Variable (name var) val

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


--------- update semantics (showing that you can't stack postits) ---------

alpha_cmp_updateVar :: Value -> Variable -> Box
-- alpha_cmp_updateVar = (\v1 -> alpha_A . updateVariable v1)
-- alpha_cmp_updateVar = (\v1 -> alpha_A . (\var -> Variable (name var) v1))
-- alpha_cmp_updateVar = (\v1 -> (\v -> Box (name v) ((Postit (value v)):[])) . (\var -> Variable (name var) v1))
-- alpha_cmp_updateVar = (\v1 -> \x -> (\v -> Box (name v) ((Postit (value v)):[])) ((\var -> Variable (name var) v1) x))
-- alpha_cmp_updateVar = (\v1 -> \x -> (\v -> Box (name v) ((Postit (value v)):[])) (Variable (name x) v1))
-- alpha_cmp_updateVar = (\v1 -> \x -> Box (name (Variable (name x) v1)) ((Postit (value (Variable (name x) v1))):[]))
-- alpha_cmp_updateVar = (\v1 -> \x -> Box (name x) ((Postit (value (Variable (name x) v1))):[]))
alpha_cmp_updateVar = (\v1 -> \x -> Box (name x) ((Postit v1):[]))

updateBox_cmp_alpha :: Value -> Variable -> Box
-- updateBox_cmp_alpha = (\v1 -> updateBox v1 . alpha_A)
-- updateBox_cmp_alpha = (\v1 -> updateBox v1 . (\v -> Box (name v) [(Postit (value v))]))
-- updateBox_cmp_alpha = (\v1 -> (\box -> Box (boxLabel box) ((Postit v1):(contents box))) . (\v -> Box (name v) [(Postit (value v))]))
-- updateBox_cmp_alpha = (\v1 -> \x -> (\box -> Box (boxLabel box) ((Postit v1):(contents box))) ((\v -> Box (name v) [(Postit (value v))]) x))
-- updateBox_cmp_alpha = (\v1 -> \x -> (\box -> Box (boxLabel box) ((Postit v1):(contents box))) (Box (name x) [(Postit (value x))]))
-- updateBox_cmp_alpha = (\v1 -> \x -> Box (boxLabel (Box (name x) [(Postit (value x))])) ((Postit v1):(contents (Box (name x) [(Postit (value x))]))))
-- updateBox_cmp_alpha = (\v1 -> \x -> Box (name x) ((Postit v1):(contents (Box (name x) [(Postit (value x))]))))
-- updateBox_cmp_alpha = (\v1 -> \x -> Box (name x) ((Postit v1):[(Postit (value x))]))
updateBox_cmp_alpha = (\v1 -> \x -> Box (name x) ((Postit v1):[(Postit (value x))]))
