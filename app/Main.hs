{-# LANGUAGE InstanceSigs #-}
module Main where
import Data.List

main :: IO ()
main = print myProgram

newtype Program = Program [Rule]
data Rule = Rule Relation [Relation]
data Relation = Relation Name [Variable]
type Name = String
data Variable = Symbol String | Value String

fact :: Relation -> Rule
fact r = Rule r []

myProgram :: Program
myProgram = Program [ fact (Relation "station" [Value "york"])
                    , Rule (Relation "reachable" [Symbol "x", Symbol "x"]) [(Relation "station" [Symbol "x"])]]

instance Show Variable where
  show :: Variable -> String
  show (Symbol s) = s
  show (Value s) = "\"" ++ s ++ "\""

instance Show Relation where
  show :: Relation -> String
  show (Relation name vars) = name ++ "(" ++ intercalate ", " (map show vars) ++ ")"

instance Show Rule where
  show :: Rule -> String
  show (Rule head []) = show head ++ "."
  show (Rule head body) = show head ++ " <-\n  " ++ intercalate ", " (map show body) ++ "."

instance Show Program where
  show :: Program -> String
  show (Program rules) = intercalate "\n" (map show rules)
