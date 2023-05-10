{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}


module NotionalMachines.Meta.Diagramable where

import Diagrams.Prelude (Backend, Diagram, V2)

class Backend b V2 Double => Diagramable a b | a -> b where
    toDiagram    ::  a  -> IO (Diagram b)
    toDiagramSeq :: [a] -> IO (Diagram b)

toDiagramSeq' :: Diagramable a b => ([Diagram b] -> Diagram b) -> [a] -> IO (Diagram b)
toDiagramSeq' cmb = fmap cmb . mapM toDiagram

