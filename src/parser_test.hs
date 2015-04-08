import qualified Text.ParserCombinators.Parsec as P
import Data.List.Split
import Data.List
import System.Exit
import System.IO
-- importing other self-written modules
import Gen

-- functions to parse the orthlogous-file
orthomclFile :: P.GenParser Char st [[String]]
orthomclFile = P.endBy line eol

line :: P.GenParser Char st [String]
line = P.sepBy cell (P.char ' ')
cell = P.many (P.noneOf " \n")
eol = P.char '\n'

report err = do
  hPutStrLn stderr ("Error: " ++ show err)
  exitFailure

parseOrthologues :: String -> IO [[String]]
parseOrthologues input = P.parseFromFile orthomclFile input >>= either report return

-- filter for the list to keep just genes with exactly one partner
getTaxCode :: String -> String
getTaxCode s = head $ splitOn "|" s

orthologues :: [String] -> Bool
orthologues xs = length xs == 3 &&
                 getTaxCode (head (tail xs)) /= getTaxCode (last xs)

-- function to convert [[String]] to [Gen]
stringToGen :: String -> Gen
stringToGen s = let
                  [org, rest] = splitOn "|" s
                  lst = splitOn "_" rest
                in convertToGen org lst

listToGenes :: [String] -> [Gen]
listToGenes [x] = [stringToGen x]
listToGenes (x : xs) = stringToGen x : listToGenes xs

orthosToGenes :: [[String]] -> [Gen]
orthosToGenes [x] = listToGenes $ tail x
orthosToGenes (x : xs) = listToGenes (tail x) ++ orthosToGenes xs

-- test the filter function
testFilterOrthos = do
  all <- parseOrthologues "../data/groups.txt"
  print $ orthosToGenes $ filter orthologues all

-- parser for gff
gffFile :: P.GenParser Char st [[String]]
gffFile = P.endBy gffline eol

gffline :: P.GenParser Char st [String]
gffline = P.sepBy gffcell (P.char '\t')
gffcell = P.many (P.noneOf "\t\n")

parseGff :: String -> IO [[String]]
parseGff input = P.parseFromFile gffFile input >>= either report return

-- filter function to get only CDS entries for the needed genes
checkLineForEAndS :: Eq a => [a] -> [a] -> [[a]] -> Bool
checkLineForEAndS s e = any (\x -> isInfixOf s x && isInfixOf e x)

checkOrthologuesForCDS :: Eq a => [a] -> [a] -> [[[a]]] -> Bool
checkOrthologuesForCDS s e = any (checkLineForEAndS s e)

filterCDS :: [[String]] -> [String] -> Bool
filterCDS orthos xs = (length xs >= 3) && ((xs !! 2) == "CDS")
                      && checkOrthologuesForCDS (xs !! 3) (xs !! 4) orthos

testFilterCDS = do
  geneInfo <- parseGff "../data/NC_012587.gff"
  orthos <- parseOrthologues "../data/rhizobium_real.txt"
  print $ length geneInfo
  let filteredOrthos = filter orthologues orthos
  print $ length $ filter (filterCDS filteredOrthos) geneInfo

main =
  testFilterCDS
