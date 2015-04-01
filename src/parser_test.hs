import qualified Text.ParserCombinators.Parsec as P
import Data.List.Split
import System.Exit
import System.IO

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
orthologues xs = and [((length xs) == 3),
                      ((getTaxCode (head (tail xs))) /= (getTaxCode (last xs)))]

main = do
  all <- parseOrthologues "../data/groups.txt"
  print $ filter orthologues all

-- parser for gff
gffFile :: P.GenParser Char st [[String]]
gffFile = P.endBy gffline eol

gffline :: P.GenParser Char st [String]
gffline = P.sepBy gffcell (P.char '\t')
gffcell = P.many (P.noneOf "\t\n")

parseGff :: String -> IO [[String]]
parseGff input = P.parseFromFile gffFile input >>= either report return

-- filter function to get only CDS entries for the needed genes
filterCDS :: [String] -> Bool
filterCDS xs = ((length xs) >= 3) && ((xs !! 2) == "CDS")

testFilterCDS = do
  all <- parseGff "../data/NC_000913_e_coli.gff"
  print $ length all
  print $ length $ filter filterCDS all
