-- Based on Boro Sitnikovski's paper

-- EBNF:
-- digit  ::= "0" | "1" | ... | "8" | "9"
-- aexp   ::= aterm | aterm relop aterm
-- number ::= - digit* | digit*
-- relop  ::= "+ | "-" | "*""
-- aterm  ::= aexp | number | var
-- var    ::= A | B | C | ... | Y | Z

import qualified Data.Map as M

data Aexp = 
  ANum Integer
  | AId Char
  | APlus Aexp Aexp
  | AMinus Aexp Aexp
  | AMult Aexp Aexp

aoptimize :: Aexp -> Aexp
aoptimize (APlus (ANum a1) (ANum a2)) = ANum (a1 + a2)
aoptimize (AMinus (ANum a1) (ANum a2)) = ANum (a1 - a2)
aoptimize (AMult (ANum a1) (ANum a2)) = ANum (a1 * a2)
aoptimize x = x

-- EBNF:
-- bexp   ::= bterm | bterm brelop bterm | aterm arelop aterm | unop bterm
-- arelop ::= "==" | "<"
-- brelop ::= "&&"
-- bterm  ::= "T" | "F"
-- unop   ::= "!"

data Bexp =
  BTrue
  | BFalse
  | BEq Aexp Aexp
  | BLe Aexp Aexp
  | BLt Aexp Aexp
  | BNot Bexp
  | BAnd Bexp Bexp

boptimize :: Bexp -> Bexp
boptimize (BEq (ANum a1) (ANum a2)) = if a1 == a2 then BTrue else BFalse
boptimize (BEq (AId v1) (AId v2)) = if v1 == v2 then BTrue else BEq (AId v1) (AId v2)
boptimize (BNot BTrue) = BFalse
boptimize (BNot BFalse) = BTrue
boptimize (BAnd BFalse _) = BFalse
boptimize (BAnd BTrue b2) = b2
boptimize x = x

type Context = M.Map Char Integer

aeval :: Context -> Aexp -> Integer
aeval ctx (AId v) = ctx M.! v -- element may not exist
aeval ctx (ANum n) = n
aeval ctx (APlus a1 a2) = aeval ctx a1 + aeval ctx a2
aeval ctx (AMinus a1 a2) = aeval ctx a1 - aeval ctx a2
aeval ctx (AMult a1 a2) = aeval ctx a1 * aeval ctx a2

beval :: Context -> Bexp -> Bool
beval ctx BTrue = True
beval ctx BFalse = False
beval ctx (BEq a1 a2) = aeval ctx a1 == aeval ctx a2
beval ctx (BLe a1 a2) = aeval ctx a1 <= aeval ctx a2
beval ctx (BLt a1 a2) = aeval ctx a1 < aeval ctx a2
beval ctx (BNot b1) = not (beval ctx b1)
beval ctx (BAnd b1 b2) = beval ctx b1 && beval ctx b2

-- let e = BNot BTrue in "Optimize: " ++ show e ++ " = " ++ show (boptimize e)
-- let e = APlus (ANum 2) (ANum 5) in "Optimize: " ++ show e ++ " = " ++ show (aoptimize e)

-- let e = APlus (AId 'X') (ANum 5) in "Optimize: " ++ show e ++ " = " ++ show (aoptimize e)
-- let e = APlus (AId 'X') (ANum 5) in show e ++ " = " ++ show (aeval (M.fromList [('X', 5)] e)
-- let e = BEq (AId 'X') (ANum 5) in show e ++ " = " ++ show (beval (M.fromList [('X', 5)] e)

data Command =
  CSkip
  | CAssign Char Aexp
  | CSequence Command Command
  | CIfElse Bexp Command Command
  | CWhile Bexp Command
  | CAssert Bexp Command Bexp

eval :: Context -> Command -> Either String Context
eval ctx CSkip = Right ctx
eval ctx (CAssign c v) = Right $ M.insert c (aeval ctx v) ctx
eval ctx (CSequence c1 c2) = let ctx' = eval ctx c1 in whenRight ctx' (\ctx'' -> eval ctx'' c2)
eval ctx (CIfElse b c1 c2) = eval ctx $ if beval ctx b then c1 else c2
eval ctx (CWhile b c) =
  if beval ctx b
  then let ctx' = eval ctx c in whenRight ctx' (\ctx'' -> eval ctx'' (CWhile b c))
  else Right ctx
eval ctx (CAssert b1 c b2) =
  if beval ctx b1
  then whenRight (eval ctx c)
    (\ctx' -> if beval ctx' b2
              then Right ctx'
              else Left "Post-condition does not match!")
  else Left "Pre-condition does not match!"


-- Z := X
-- Y := 1
-- while (~Z = 0)
--   Y := Y * Z
--   Z := Z - 1

-- fact_X =
--   let l1 = CAssign 'Z' (AId 'X')
--       l2 = CAssign 'Y' (Num 1)
--       l3 = CWhile (BNot (BEq (AId 'Z') (ANum 0))) (CSequence l4 l5)
--       l4 = CAssign 'Y' (AMult (AId 'Y) (AId 'Z'))
--       l5 = CAssign 'Z' (AMinus (AId 'Z') (ANum 1))
--   in CSequence l1 (CSequence l2 l3)
--
-- eval (M.fromList [('X',5)]) fact_X
--
-- let e = CAssert (BEq (ANum 5) (AId 'X')) fact_X (BEq (ANum 120) (AId 'Y'))
-- in "Assert {X=5} fact_X {Y=120}: " ++ show (eval (M.fromList [('X', 5)]) e)
--
-- let e = CAssert (BEq (ANum 4) (AId 'X')) fact_X (BEq (ANum 120) (AId 'Y'))
-- in "Assert {X=4} fact_X {Y=120}: " ++ show (eval (M.fromList [('X', 5)]) e)
-- 