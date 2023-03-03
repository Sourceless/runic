module Main where
import Parser (parse)
import Eval.Naive (eval)
import System.Environment

runProgram :: String -> IO ()
runProgram program = do
  either print print (parse program)
  either print print (fmap eval (parse program))


main :: IO ()
main = do
  args <- getArgs
  program <- readFile (head args)
  runProgram program
