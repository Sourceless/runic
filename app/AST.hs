{-# LANGUAGE InstanceSigs #-}

module AST where
import Data.List (intercalate)

newtype Program = Program [Rule]
data Rule = Rule Relation [Assertion] deriving (Eq, Ord)
data Relation = Relation Name [Variable] deriving (Eq, Ord)
type Name = String
data Variable = Symbol String | Value String deriving (Eq, Ord)
data Assertion = Id Relation | Not Relation | Succeed | Fail deriving (Eq, Ord)

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

instance Show Assertion where
  show :: Assertion -> String
  show (Id r) = show r
  show (Not r) = "not " ++ show r
  show Succeed = "succeed"
  show Fail = "fail"
