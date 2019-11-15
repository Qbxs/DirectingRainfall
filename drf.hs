import Tarps
import InputParser (Input(..), Range, Point, Tarp(..), lexer, inputParser)
import System.Environment
import System.Exit

main :: IO()
main = getArgs >>= parse >>= (print . (map simplify) . sortInput . inputParser . lexer)

parse ["-h"]        = usage   >> exit
parse ["--help"]    = usage   >> exit
parse ["-v"]        = version >> exit
parse ["--version"] = version >> exit
parse []            = getContents
parse fs            = concat `fmap` mapM readFile fs

usage   = putStrLn "Usage: drf [-vh] [file ..]"
version = putStrLn "Haskell drf 0.1 2019"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)
