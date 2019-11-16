import Tarps
import InputParser (Input(..), Range, Point, Tarp(..), lexer, inputParser)
import System.Environment
import System.Exit
import System.Process

main :: IO()
main = getArgs >>= parse >>= (print . (map simplify) . sortInput . inputParser . lexer)

parse :: [String] -> IO String
parse ["-h"]        = usage   >> exit
parse ["--help"]    = usage   >> exit
parse ["-v"]        = version >> exit
parse ["--version"] = version >> exit
parse ["-i"]        = input   >> exit
parse ["--input"]   = input   >> exit
parse []            = getContents
parse fs            = concat `fmap` mapM readFile fs

usage, version :: IO()
usage   = putStrLn "Usage: drf [-vhi] [file ..]"
version = putStrLn "Haskell drf 0.1 2019"
input   = system "open inputCanvas.html"

exit, die :: IO String
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)
