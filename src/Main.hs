import Tarps hiding (simplify)
import InputParser (Input(..), Range, Point, Tarp(..), lexer, inputParser)
import Exports
import Data.Aeson
import GHC.Generics
import System.Environment
import System.Exit hiding (die)
import System.Process
import System.Directory

main :: IO()
main = getArgs >>= parse >>= (print . Tarps.solution . inputParser . lexer)

parse :: [String] -> IO String
parse ["-h"]            = usage    >>  exitSuccess
parse ["--help"]        = usage    >>  exitSuccess
parse ["-v"]            = version  >>  exitSuccess
parse ["--version"]     = version  >>  exitSuccess
parse ("-i":fs)         = parse fs >>= input
parse ("--input":fs)    = parse fs >>= input
parse ("-s":fs)         = parse fs >>= simplify
parse ("--simplify":fs) = parse fs >>= simplify
parse ("-w":fs)         = parse fs >>= weighing
parse ("--weigh":fs)    = parse fs >>= weighing
parse []                = getContents
parse fs                = concat `fmap` mapM readFile fs

usage, version :: IO()
usage   = do
  putStrLn "Solves for given input file the minimum amount of holes to be put in the tarps."
  putStrLn "Usage:"
  putStrLn "    - cabal run ..            Calculate an optimal solution"
  putStrLn "    - cabal run -- -i ..      Show input tarps in browser"
  putStrLn "    - cabal run -- -s ..      Show simplified tarps in browser"
  putStrLn "    - cabal run -- -w ..      Show weighted tarps in browser"
version = putStrLn "Haskell DirectingRainfall 0.1 2019 Pascal Engel"

input,simplify,weighing :: String -> IO String
input fs = do
  let inp = fs
  (writeInput . inJ . inputParser . lexer) inp
  putStrLn "Opening inputCanvas.html"
  system "open js/inputCanvas.html"
  exitSuccess

simplify fs = do
  let inp = fs
  (writeSimple . siJ . inputParser . lexer) inp
  putStrLn "Opening simpleCanvas.html"
  system "open js/simpleCanvas.html"
  exitSuccess

weighing fs = do
  let inp = fs
  (writeWeighted . weJ . inputParser . lexer) inp
  putStrLn "Opening weightedCanvas.html"
  system "open js/weightedCanvas.html"
  exitSuccess
