{-# LANGUAGE InstanceSigs #-}
module Main where
import Data.List (intercalate)
import Text.ParserCombinators.Parsec

newtype Program = Program [Rule]
data Rule = Rule Relation [Relation]
data Relation = Relation Name [Variable]
type Name = String
data Variable = Symbol String | Value String

program :: GenParser Char st Program
program =
    do result <- many rule
       eof
       return (Program result)

rule :: GenParser Char st Rule
rule =
    do head <- relation
       spaces
       body <- ruleBody
       spaces
       char '.'
       spaces
       return (Rule head body)

ruleBody :: GenParser Char st [Relation]
ruleBody =
    option [] (do
       implies
       sepBy relation (char ',')
    )

implies :: GenParser Char st ()
implies = do
    spaces
    string "<-"
    spaces
    return ()

relation :: GenParser Char st Relation
relation = 
    do name <- many letter
       char '('
       spaces
       variables <- sepBy variable (char ',')
       spaces
       char ')'
       return (Relation name variables)

variable :: GenParser Char st Variable
variable = do
    spaces
    var <- (rValue <|> rSymbol)
    spaces
    return var

rSymbol :: GenParser Char st Variable
rSymbol = do
    name <- many letter
    return (Symbol name)
    
rValue :: GenParser Char st Variable
rValue = do
    val <- between (char '"') (char '"') (many (noneOf ['"']))
    return (Value val)


myProgram :: Program
myProgram = Program [ Rule (Relation "station" [Value "york"]) []
                    , Rule (Relation "reachable" [Symbol "x", Symbol "x"]) [(Relation "station" [Symbol "x"])]]

myProgramText :: String
myProgramText = "station(\"york\").\nreachable(x, x) <-\nstation(x)."

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

parseRunic :: String -> Either ParseError Program
parseRunic input = parse program "(unknown)" input

main :: IO ()
main = print (parseRunic myProgramText)