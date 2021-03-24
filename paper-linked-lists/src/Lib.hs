{-# LANGUAGE TupleSections #-}

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

--------------------
---- Data Types ------
--------------------

--------------------
-- Expression     -- A', B'
type LinkedList = [String]

--------------------
-- Expression Tree Diagram -- A , B
data LinkedListDiagram = LinkedListDia { objects :: [Object] } deriving (Show, Eq)
data Object = Object { typ :: Name
                     , vars :: [Var] }
            | Null deriving (Show, Eq)
data Var = Var { name :: Name
               , val :: Val } deriving (Show, Eq)
data Val = Str String | Num Int | Ref Object deriving (Show, Eq)
type Name = String


list2dia :: LinkedList -> LinkedListDiagram
list2dia lst = LinkedListDia $ case list2nodes lst of
                             [] -> [mkList 0 Null]
                             xs -> (mkList (length xs) (head xs)):xs
  where list2nodes [] = []
        list2nodes (x:xs) = case list2nodes xs of
                              []            -> (mkNode (Str x) Null):[]
                              nodes @ (y:_) -> (mkNode (Str x) y   ):nodes

        mkList :: Int -> Object -> Object
        mkList size first = Object "List" [ Var "size" (Num size)
                                          , Var "head" (Ref first) ]
        mkNode :: Val -> Object -> Object
        mkNode dta nxt = Object "ListNode" [ Var "data" dta
                                           , Var "next" (Ref nxt) ]

dia2list :: LinkedListDiagram -> Maybe LinkedList
dia2list (LinkedListDia objs) = d2l objs
  where
    d2l :: [Object] -> Maybe [String]
    d2l ((Object "List" [Var "size" (Num 0), Var "head" (Ref Null)]):[]) = Just []
    d2l ((Object "List" [Var "size" (Num s), Var "head" (Ref r   )]):o:os)
      | s == length (o:os) && r == o = node2l (o:os)
    d2l _ = Nothing

    node2l :: [Object] -> Maybe [String]
    node2l ((Object "ListNode" [Var "data" (Str s), Var "next" (Ref Null)]):[]) = Just (s:[])
    node2l ((Object "ListNode" [Var "data" (Str s), Var "next" (Ref next)]):n:ns)
      | next == n = (s:) <$> node2l (n:ns)
    node2l _ = Nothing


------------------

--    A  --f-->  B
--
--    ^          ^
--    |          |
--  alphaA    alphaB
--    |          |
--    |          |
--
--    A' --f'--> B'

type A' = LinkedList
type B' = LinkedList

type A  = LinkedListDiagram
type B  = Maybe LinkedListDiagram

f' :: LinkedList -> LinkedList
f' = map (show . length)

alphaA :: LinkedList -> LinkedListDiagram
alphaA = list2dia

f :: LinkedListDiagram -> Maybe LinkedListDiagram
f = fmap (list2dia . f') . dia2list

alphaB :: LinkedList -> Maybe LinkedListDiagram
alphaB = Just . list2dia


-- Commutation proof:
-- alpha_B . f' == f . alpha_A

alphaBf' :: A' -> B
alphaBf' = error "to be implemented"

falphaA :: A' -> B
falphaA = error "to be implemented"

