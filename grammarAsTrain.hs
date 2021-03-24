module GrammarAsTrainTrack where

import Test.QuickCheck

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
--
--       A  - Abstract representation (E.g., abstract data structure: List) == Notional machine
--
--       A' - Concrete representation (E.g., concrete data structure: ArrayList) == Programming language
--
--       f  - Abstract program == Notional machine "process"
--
--       f' - Program state transition function (e.g. reduction)
--
--  alpha_X - Abstraction Function
--
--
-- The abstraction is correct if:
-- alpha_B . f' == f . alpha_A

-----------------

type A' = 
  (CFG Char
  ,SList Char -- Processed (recognized)
  ,Input -- Still to be recognized
  ,Stack -- Symbols of the grammar that still need to be processed
  )
-- Note that there is a choice here: 
-- Instead of the processed input and the stack, I could also store the Parse tree of the part of the input that has been processed   
-- I don't think the A' representation is very important: anything that represents the formal components is fine
-- It is the notional machine that is important, that is the component we want to study

type B' = Maybe A'
-- Also here is a choice: 
-- I can either represent the entire parsing process, and define the function f' as parse
-- or I can represent a single parsing step, represented by parseT(erminal) and parseN(onterminal)
-- I've chosen for the latter; that makes it more like a machine that takes steps
-- similar to the train that moves through a tunnel.
-- Maybe is added to represent potentially failing parsing steps

parseT :: A' -> B'
parseT (_,_,[],_)    =  Nothing
parseT (g,r,t:ts,s:ss) 
  | isT s && t == s  =  Just (g,Snoc r t,ts,ss) 
  | otherwise        =  Nothing

parseN :: A' -> B'
parseN (g,r,ts,s:ss) 
  | isN s            =  case parseRhss (g,r,ts,selectRhss (snd g) s) of
                             Just (g',r',ts',[]) -> Just (g',r',ts',ss)
                             Nothing             -> Nothing
  | otherwise        =  Nothing

parseRhss (g,r,ts,[]) = Nothing
parseRhss (g,r,ts,rhs:rhss) = case parseRhs (g,r,ts,rhs) of
                                Just q -> Just q
                                Nothing -> parseRhss (g,r,ts,rhss)

parseRhs (g,r,ts,[]) = Just (g,r,ts,[])
parseRhs (g,r,ts,s:ss) 
  | isT s = case parseT (g,r,ts,s:ss) of
              Just (g',r',ts',ss') -> parseRhs (g',r',ts',ss')
              Nothing -> Nothing
  | isN s = case parseN (g,r,ts,s:ss) of
              Just (g',r',ts',ss') -> parseRhs (g',r',ts',ss')
              Nothing -> Nothing


type A = (LocationTrain,Input)

type B = Maybe A

alphaA :: A' -> A
alphaA = draw 

alphaB :: B' -> B
alphaB = maybe Nothing (Just . draw) 

draw :: A' -> A
draw (g,r,ts,ss) = 
  let start = cfg2traintrack g
      tt = case start of NTunnel c r -> r -- removed start symbol nonterminal tunnel 
      l = rev r 
  in (trip (tt,Top) l,ts)

-- trip takes a LocationTrain, and a number of input characters, 
-- and moves the train through tunnels to obtain the position of the train 
-- after parsing the input.
trip :: LocationTrain -> [Char] -> LocationTrain
trip t [] = t
trip t (x:xs) = case move t x of
                  Just t' -> trip t' xs
                  Nothing -> error "Should be a parsing"


-- cfg2traintrack takes a CFG, and produces a Tunnel for the start symbol,
-- including a traintrack for its rhss
cfg2traintrack :: CFG Char -> Tunnel 
cfg2traintrack g = nt2traintrack (fst g) g

nt2traintrack :: Char -> CFG Char -> Tunnel
nt2traintrack s g = 
  let rhss = selectRhss (snd g) s
      rhss2tt = makeSwitch $ map (makeSequence . map (\c -> if isT c then TTunnel c else nt2traintrack c g)) rhss
  in NTunnel s rhss2tt
  
makeSequence :: [Tunnel] -> TrainTrack
makeSequence = foldr Sequence End

makeSwitch :: [TrainTrack] -> TrainTrack
makeSwitch = foldr1 Switch 
    
move :: (TrainTrack,ContextTrainTrack) -> Char -> Maybe (TrainTrack,ContextTrainTrack) 
move (Sequence (TTunnel c) r,ctt) c' = if c==c' then Just (r,CSequence ctt (CTTunnel c)) else Nothing
move (Sequence (NTunnel c tt) r,ctt) c' = 
  case move (tt,CSequence ctt (CNTunnel c)) c' of
      Just (p,q) -> Just (concatSeq p r,q)
      Nothing -> if noTTunnels tt then move (r,CSequence ctt (CNTunnel c)) c' else Nothing
move (Switch t1 t2,ctt) c = undefined

noTTunnels = undefined
concatSeq = undefined

desired_property_T, desired_property_N :: A' -> Bool
desired_property_T (g,r,i,s) = 
  let (l,i') = alphaA (g,r,i,s) 
  in alphaB (parseT (g,r,i,s)) == Just (unJust $ move l (head i'),tail i')
desired_property_N a = undefined -- (alphaB . parseN) a == (move . alphaA) a

unJust (Just i) = i

main = quickCheck desired_property_T


-- Auxiliary types

data SList a = SNil | Snoc (SList a) a deriving Show
-- Lists that add elements at the end 
-- used to represent the input characters that have been processed

rev :: SList a -> [a]
rev SNil = []
rev (Snoc l x) = x:rev l

-- Types for context-free grammars

type CFG s = (s,[(s,[s])])

class Eq s => Symbol s where 
  isT :: s -> Bool
  isN :: s -> Bool
  isT = not . isN

instance Symbol Char where 
  isN c = 'A'<=c && c<='Z'

exGrammar  :: CFG Char
exGrammar  = 
       ('S'
       , [('S',"AaS"),('S',"B"),('S',"CB")
         ,('A',"SC"),('A',"")
         ,('B',"A"),('B',"b")
         ,('C',"D")
         ,('D',"d")
         ]
       )

selectRhss [] s = []
selectRhss ((s',rhs):prods) s = let rhss = selectRhss prods s 
                                in if s'==s then rhs:rhss else rhss

type Input = [Char]

type Stack = [Char]


-- Types for train tracks

type LocationTrain = (TrainTrack,ContextTrainTrack)

data TrainTrack = 
    Sequence Tunnel TrainTrack
  | Switch TrainTrack TrainTrack
  | Repeat TrainTrack
  | End 
  deriving (Eq,Show)

data Tunnel = NTunnel Char TrainTrack | TTunnel Char deriving (Eq,Show)

data ContextTrainTrack = 
    Top
  | CSequence ContextTrainTrack CTunnel
  | CSwitchR ContextTrainTrack TrainTrack
  | CSwitchL TrainTrack ContextTrainTrack
  | CRepeat ContextTrainTrack
  deriving (Eq,Show)
  
data CTunnel = CNTunnel Char | CTTunnel Char deriving (Eq,Show) 
  

-- QuickCheck

instance Arbitrary a => Arbitrary (SList a) where
  arbitrary = undefined