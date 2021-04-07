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

-- I want to model stack-based top-down parsing, with context-free grammars that are not left recursive. 
-- Concrete representation:

type A' = 
  (CFG Char
  ,SList Char -- Processed (recognized)
  ,Input      -- Still to be recognized
  ,Stack      -- Symbols of the grammar that still need to be processed
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

-- parseT parses a single terminal, which should appear both at the head of the input and the head of the stack.
-- its corresponding function in the NM is move
parseT :: A' -> B'
parseT (_,_,[],_)    =  Nothing
parseT (g,r,t:ts,s:ss) 
  | isT s && t == s  =  Just (g,Snoc r t,ts,ss) 
  | otherwise        =  Nothing

-- parseN parses a nonterminal by trying to parse with all its right-hand sides
-- its corresponding function in the NM is trip
parseN :: A' -> B'
parseN (g,r,ts,s:ss) 
  | isN s            =  case parseRhss (g,r,ts,selectRhss (snd g) s) of
                             Just (g',r',ts',[]) -> Just (g',r',ts',ss)
                             Nothing             -> Nothing
  | otherwise        =  Nothing

parseRhss :: (CFG Char, SList Char, Input, [Stack]) -> B'
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

-- The NM: the abstract representation.

-- Types for train tracks

type LocationTrain = (TrainTrack,ContextTrainTrack)
-- For each nonterminal we have a train track.
-- The train is at a location of a particular traintrack.
-- This location is modeled by a zipper.
-- The tuple denotes: the rest of the train track that still needs to be passed, 
-- and the part of the train track that has already been passed

data TrainTrack = 
    Sequence Tunnel TrainTrack
  | Switch TrainTrack TrainTrack
  | Repeat TrainTrack
  | End 
  deriving (Eq,Show)

data Tunnel = NTunnel Char | TTunnel Char deriving (Eq,Show)

data ContextTrainTrack = 
    Top
  | CSequence ContextTrainTrack Tunnel
  | CSwitchR ContextTrainTrack TrainTrack
  | CSwitchL TrainTrack ContextTrainTrack
  | CRepeat ContextTrainTrack
  deriving (Eq,Show)

type A = ([LocationTrain],Input)

type B = Maybe A

alphaA :: A' -> A
alphaA = draw 

alphaB :: B' -> B
alphaB = maybe Nothing (Just . draw) 

draw :: A' -> A
draw (g,r,ts,ss) = 
  let nts = nonterminals g 
      nttracks = map (drawnt g) nts 
      start = fst g
      current = select start nttracks
      l = reverse (toList r) 
  in (trip [(current,Top)] l nttracks,ts)
  
drawnt :: CFG Char -> Char -> (Char,TrainTrack)
drawnt g c = 
  let rhss = selectRhss (snd g) c
      rhss2tt = makeSwitch $ map (makeSequence . map (\c -> if isT c then TTunnel c else NTunnel c)) rhss
  in (c,rhss2tt)

makeSequence :: [Tunnel] -> TrainTrack
makeSequence = foldr Sequence End

makeSwitch :: [TrainTrack] -> TrainTrack
makeSwitch = foldr1 Switch 

select :: Char -> [(Char,TrainTrack)] -> TrainTrack
select c nttracks = head [ntt | (c',ntt) <- nttracks, c==c']

-- trip takes a list of LocationTrains, where the train is in the first
-- LocationTrain, and returns to the next one if it finishes the first.
-- and a number of input characters, and moves the train through tunnels 
-- to obtain the position of the train after parsing the input.
trip :: [LocationTrain] -> [Char] -> [(Char,TrainTrack)] -> [LocationTrain]
trip lts [] ntts = lts
trip lts (x:xs) ntts = case move lts x ntts of
                              Just lts' -> trip lts' xs ntts
                              Nothing -> error "trip: Should be a parsing"

-- move takes a list of train locations (each train location respresenting a nonterminal in the current parse tree), 
-- a character, and the set of all train tracks (one per nonterminal of the grammar)
-- and moves the train a single step.
move :: [LocationTrain] -> Char -> [(Char,TrainTrack)] -> Maybe [LocationTrain]
move [] c ntts = error "move: Should be a parsing"
move ((tt,ctt):lts) c ntts = case tt of 
  Sequence (TTunnel c') r -> if c==c' then Just ((r,CSequence ctt (TTunnel c)):lts) else Nothing
  Sequence (NTunnel c') r -> move ((select c' ntts,Top):(r,CSequence ctt (NTunnel c')):lts) c ntts 
  Switch t1 t2            -> case move ((t1,CSwitchR ctt t2):lts) c ntts of
                               Just lts' -> Just lts'
                               Nothing -> move ((t2,CSwitchL t1 ctt):lts) c ntts
  Repeat tt               -> case move ((tt,CRepeat ctt):lts) c ntts of
                               Just lts' -> Just lts'
                               Nothing -> move ((End,CRepeat ctt):lts) c ntts
  End                     -> move lts c ntts                                                         

g1 :: CFG Char
g1 = ('S'
     ,[('S',"aA")
      ,('A',"b"),('A',"")
      ]
     )

g2 :: CFG Char
g2 = ('S'
     ,[('S',"aA")
      ,('A',"b"),('A',"S")
      ]
     )
				
exnts1 = nonterminals g1
exNttracks1 = map (drawnt g1) exnts1 

exnts2 = nonterminals g2
exNttracks2 = map (drawnt g2) exnts2 

desired_property_T, desired_property_N :: A' -> Bool
desired_property_T (g,r,i,s) = 
  let (l,i') = alphaA (g,r,i,s)
      nts = nonterminals g 
      nttracks = map (drawnt g) nts 
  in alphaB (parseT (g,r,i,s)) == Just (unJust $ move l (head i') nttracks,tail i')

exArgsT1 = (g1, Snil, "ab" , "aA")
exParseT1 = parseT exArgsT1
exAlphaBT1 = alphaB exParseT1
exAlphaAT1 = alphaA exArgsT1
exMoveT1 = move (fst exAlphaAT1) (head (snd exAlphaAT1)) exNttracks1
exDesiredPropertyT1 = exAlphaBT1 == Just (unJust exMoveT1,tail (snd exAlphaAT1))

exArgsT2 = (g2, Snil, "aab" , "aA")
exParseT2 = parseT exArgsT2
exAlphaBT2 = alphaB exParseT2
exAlphaAT2 = alphaA exArgsT2
exMoveT2 = move (fst exAlphaAT2) (head (snd exAlphaAT2)) exNttracks2
exDesiredPropertyT2 = exAlphaBT2 == Just (unJust exMoveT2,tail (snd exAlphaAT2))

unJust (Just i) = i

desired_property_N (g,r,i,s) = 
   let (l,_) = alphaA (g,r,i,s)
       nts = nonterminals g 
       nttracks = map (drawnt g) nts 
       p = parseN (g,r,i,s)
       ts = case p of Just (g',r',i',s') -> take (length i - length i') i 
   in fst (unJust (alphaB (parseN (g,r,i,s)))) == trip l ts nttracks

exArgsN1 = (g1, Snil, "ab" , "S") 
exParseN1 = parseN exArgsN1
exAlphaBN1 = alphaB exParseN1
exts1 = case exParseN1 of Just (g',r',i',s') -> take (length "ab" - length i') "ab" 
exAlphaAN1 = alphaA exArgsN1
exTrip1 = trip (fst exAlphaAN1) exts1 exNttracks1
exDesiredPropertyN1 = fst (unJust exAlphaBN1) == exTrip1

exArgsN2 = (g2, Snil, "aab" , "S") 
exParseN2 = parseN exArgsN2
exAlphaBN2 = alphaB exParseN2
exts2 = case exParseN1 of Just (g',r',i',s') -> take (length "aab" - length i') "aab" 
exAlphaAN2 = alphaA exArgsN2
exTrip2 = trip (fst exAlphaAN2) exts2 exNttracks2
exDesiredPropertyN2 = fst (unJust exAlphaBN2) == exTrip2

main = quickCheck desired_property_T



-- Auxiliary types

data SList a = Snil | Snoc (SList a) a deriving Show
-- Lists that add elements at the end 
-- used to represent the input characters that have been processed

toList :: SList a -> [a]
toList Snil = []
toList (Snoc l x) = x:toList l

-- Types for context-free grammars

type CFG s = (s,[(s,[s])])

class Eq s => Symbol s where 
  isT :: s -> Bool
  isN :: s -> Bool
  isT = not . isN

instance Symbol Char where 
  isN c = 'A'<=c && c<='Z'

nonterminals g = nub (fst g:map fst (snd g))

selectRhss [] s = []
selectRhss ((s',rhs):prods) s = let rhss = selectRhss prods s 
                                in if s'==s then rhs:rhss else rhss

type Input = [Char]

type Stack = [Char]


  
-- QuickCheck

instance Arbitrary a => Arbitrary (SList a) where
  arbitrary = undefined

-- Auxiliary functions

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x:nub (filter (/=x) xs)

-- Examples

-- left-recursive grammars won't work (of course)
exGrammar  :: CFG Char
exGrammar  = 
       ('S'
       , [('S',"aS"),('S',"B"),('S',"CB")
         ,('A',"SC"),('A',"")
         ,('B',"A"),('B',"b")
         ,('C',"D")
         ,('D',"d")
         ]
       )

