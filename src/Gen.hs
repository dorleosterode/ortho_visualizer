module Gen (Gen, Organism, Start, End, Direction, Id, getOrganism, getId, getStart, getEnd, getDirection,
               setOrganism, setId, setStart, setEnd, setDirection,
               convertToGen) where

type Organism = String
type Start = Int
type End = Int
type Id = Int
type Direction = Bool

data Gen = Gen Organism Id Start End Direction deriving (Eq, Show)

-- functions to get information
getOrganism (Gen org id s e dir) = org
getId (Gen org id s e dir) = id
getStart (Gen org id s e dir) = s
getEnd (Gen org id s e dir) = e
getDirection (Gen org id s e dir) = dir

-- functions to set the information new
setOrganism (Gen org id s e dir) n = (Gen n id s e dir)
setId (Gen org id s e dir) n = (Gen org n s e dir)
setStart (Gen org id s e dir) n = (Gen org id n e dir)
setEnd (Gen org id s e dir) n = (Gen org id s n dir)
setDirection (Gen org id s e dir) n = (Gen org id s e n)

-- functions to convert gens from different strings to genes
convertToGen :: String -> [String] -> Gen
convertToGen org [c, id] = (Gen org (read id) 0 0 True)
convertToGen org [_, _, s, e, "1"] = (Gen org 0 (read s) (read e) True)
