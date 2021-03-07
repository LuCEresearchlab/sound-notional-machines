module GrammarAsTrainTrack where

import Test.QuickCheck

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
parseT (g,r,[],s)    =  Nothing
parseT (g,r,t:ts,s:ss) 
  | isT s && t == s  =  Just (g,Snoc r t,ts,ss) 
  | otherwise        =  Nothing

parseN :: A' -> B'
parseN = undefined 

type A = (LocationTrain,Input)

type B = Maybe A

alphaA :: A' -> A
alphaA = draw 

alphaB :: B' -> B
alphaB = maybe Nothing (Just . draw) 

draw :: A' -> A
draw = undefined -- recursive def over grammar

move :: A -> B
move = undefined

desired_property :: A' -> Bool
desired_property a = (alphaB . parseT) a == (move . alphaA) a

main = quickCheck desired_property


-- Auxiliary types

data SList a = SNil | Snoc (SList a) a deriving Show
-- Lists that add elements at the end 
-- used to represent the input characters that have been processed


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

type Input = [Char]

type Stack = [Char]


-- Types for train tracks

type LocationTrain = (TrainTrack,ContextTrainTrack)

data TrainTrack = 
    Sequence Tunnel TrainTrack
  | Switch TrainTrack TrainTrack
  | Repeat TrainTrack
  | End 
  deriving Eq

data Tunnel = NTunnel Char | TTunnel Char deriving Eq

data ContextTrainTrack = 
    Top
  | CSequence Tunnel ContextTrainTrack
  | CSwitchR ContextTrainTrack TrainTrack
  | CSwitchL TrainTrack ContextTrainTrack
  | CRepeat ContextTrainTrack
  deriving Eq
  

-- QuickCheck

instance Arbitrary a => Arbitrary (SList a) where
  arbitrary = undefined