import Tarps
import InputParser (Input(..), Range, Point, Tarp(..), lexer, inputParser)
import Exports
import Data.Aeson
import GHC.Generics
import System.Environment
import System.Exit
import System.Process
import System.Directory

main :: IO()
main = getArgs >>= parse >>= (print . (map simplify) . sortInput . inputParser . lexer)

parse :: [String] -> IO String
parse ["-h"]             = usage   >> exit
parse ["--help"]         = usage   >> exit
parse ["-v"]             = version >> exit
parse ["--version"]      = version >> exit
parse ("-i":fs)          = (parse fs) >>= input
parse ("--input":fs)     = (parse fs) >>= input
parse []                 = getContents
parse fs                 = concat `fmap` mapM readFile fs

usage, version :: IO()
usage   = putStrLn "Usage: drf [-vhi] [file ..]"
version = putStrLn "Haskell drf 0.1 2019"

input :: String -> IO String
input fs = do
  inp <- return fs
  (writeInput . inJ . inputParser . lexer) inp
  system "open js/inputCanvas.html"
  putStrLn "Succesfully opened inputCanvas.html"
  exit

exit, die :: IO String
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)

scnd :: [String]-> IO [String]
scnd xs = return $ tail xs
