{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where
import Text.RawString.QQ ( r )
import Parser (parse)
import Eval.Naive (eval)
import System.Environment

myProgramText :: String
myProgramText = [r|
station("york").
station("leeds").
station("manchester").

linked("york", "leeds").
linked("leeds", "manchester").

linked(x, y) <-
  linked(y, x).

reachable(x, x) <-
  station(x).

reachable(x, y) <-
  station(x),
  station(y),
  linked(x, y).

reachable(x, z) <-
  reachable(x, y),
  reachable(y, z).
|]

runProgram :: String -> IO ()
runProgram program = do
  either print print (fmap eval (parse program)) -- TODO how do i refactor this


main :: IO ()
main = do
  args <- getArgs
  program <- readFile (head args)
  runProgram program
