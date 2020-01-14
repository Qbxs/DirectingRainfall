import qualified Tarps as T
import InputParser (Input(..), Range, Point, Tarp(..), lexer, inputParser)
import Exports
import Data.Aeson
import GHC.Generics
import System.Environment
import System.Exit
import System.Process
import System.Directory

main :: IO()
main = getArgs >>= parse >>= (print . T.solution . inputParser . lexer)

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
parse fs                = concat <$> mapM readFile fs

usage, version :: IO()
usage   = do
  putStrLn "Solves for given input file the minimum amount of holes to be put in the tarps."
  putStrLn "Usage: (.. file)"
  putStrLn "        ..      Calculate an optimal solution"
  putStrLn "     -i ..      Show input tarps in browser"
  putStrLn "     -s ..      Show simplified tarps in browser"
  putStrLn "     -w ..      Show weighted tarps in browser"
version = putStrLn "Haskell DirectingRainfall 1.0 2019 Pascal Engel"

input,simplify,weighing :: String -> IO String
input fs = do
  (writeInput . inJ . inputParser . lexer) fs
  putStrLn "Opening inputCanvas.html"
  system "open js/inputCanvas.html"
  exitSuccess

simplify fs = do
  (writeSimple . siJ . inputParser . lexer) fs
  putStrLn "Opening simpleCanvas.html"
  system "open js/simpleCanvas.html"
  exitSuccess

weighing fs = do
  (writeWeighted . weJ . inputParser . lexer) fs
  putStrLn "Opening weightedCanvas.html"
  system "open js/weightedCanvas.html"
  exitSuccess
