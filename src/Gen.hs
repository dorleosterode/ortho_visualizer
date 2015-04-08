module Gen (Gen, Organism, Start, End, Direction, getOrganism, getStart, getEnd, getDirection,
               setOrganism, setStart, setEnd, setDirection) where

type Organism = String
type Start = Int
type End = Int
type Direction = Bool

data Gen = Gen Organism Start End Direction deriving (Eq, Show)

-- functions to get information
getOrganism (Gen org s e dir) = org
getStart (Gen org s e dir) = s
getEnd (Gen org s e dir) = e
getDirection (Gen org s e dir) = dir

-- functions to set the information new
setOrganism (Gen org s e dir) n = (Gen n s e dir)
setStart (Gen org s e dir) n = (Gen org n e dir)
setEnd (Gen org s e dir) n = (Gen org s n dir)
setDirection (Gen org s e dir) n = (Gen org s e n)
