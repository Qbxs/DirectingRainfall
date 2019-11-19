import Tarps hiding (simplify)
import InputParser (Input(..), Range, Point, Tarp(..), lexer, inputParser)
import Exports
import Data.Aeson
import GHC.Generics
import System.Environment
import System.Exit
import System.Process
import System.Directory

main :: IO()
main = getArgs >>= parse >>= (print . sortInput . inputParser . lexer)

parse :: [String] -> IO String
parse ["-h"]             = usage   >> exit
parse ["--help"]         = usage   >> exit
parse ["-v"]             = version >> exit
parse ["--version"]      = version >> exit
parse ("-i":fs)          = (parse fs) >>= input
parse ("--input":fs)     = (parse fs) >>= input
parse ("-s":fs)          = (parse fs) >>= simplify
parse ("--simplify":fs)  = (parse fs) >>= simplify
parse []                 = getContents
parse fs                 = concat `fmap` mapM readFile fs

usage, version :: IO()
usage   = do
  putStrLn "Solves for given input file the minimum amount of holes to be put in the tarps."
  putStrLn "Usage:"
  putStrLn "    - run cabal ..            Calculate an optimal solution"
  putStrLn "    - run cabal -- -i ..      Show input tarps in browser"
  putStrLn "    - run cabal -- -s ..      Show simplified tarps in browser"
  putStrLn "    - run cabal -- -w ..      Show weighted tarps in browser"
version = putStrLn "Haskell DirectingRainfall 0.1 2019 Pascal Engel"

input,simplify :: String -> IO String
input fs = do
  inp <- return fs
  (writeInput . inJ . inputParser . lexer) inp
  putStrLn "Opening inputCanvas.html"
  system "open js/inputCanvas.html"
  exit

simplify fs = do
  inp <- return fs
  (writeSimple . siJ . inputParser . lexer) inp
  putStrLn "Opening simpleCanvas.html"
  system "open js/simpleCanvas.html"
  exit

exit, die :: IO String
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)
