import Text.ParserCombinators.Parsec
import System.Exit
import System.IO

orthomclFile :: GenParser Char st [[String]]
orthomclFile = endBy line eol

line :: GenParser Char st [String]
line = sepBy cell (char ' ')
cell = many (noneOf " \n")
eol = char '\n'

report err = do
  hPutStrLn stderr ("Error: " ++ show err)
  exitFailure

parseOrthologues :: String -> IO [[String]]
parseOrthologues input = parseFromFile orthomclFile input >>= either report return
