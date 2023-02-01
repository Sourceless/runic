{-# LANGUAGE InstanceSigs #-}
module Main where
import Data.List (intercalate)
import Text.ParserCombinators.Parsec
import Data.Set (Set, empty, unions, fromList, union, filter, singleton)

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
parseRunic = parse program "(unknown)"

data Fact = Fact String [String] deriving (Show, Eq, Ord)

runRunic :: Program -> Set Fact
runRunic p = runRunic' p empty


runRunic' :: Program -> Set Fact -> Set Fact
runRunic' p facts =
  let facts' = runRunicStep p facts in
        if facts == facts'
        then facts'
        else runRunic' p facts'

runRunicStep :: Program -> Set Fact -> Set Fact
runRunicStep (Program rules) facts = unions (map (runRunicRule facts) rules)

-- using facts we know and an existing rule, derive new facts
runRunicRule :: Set Fact -> Rule -> Set Fact
runRunicRule facts (Rule (Relation name vars) []) =
  -- if we are out of relations in the body, if all the vars have unified, create a new fact
  let rule = Rule (Relation name vars) [] in
        if ruleSatisfied rule
        then facts `union` singleton (toFact rule)
        else facts
runRunicRule facts (Rule (Relation name vars) rules) = _
  -- go through each fact we have and try to apply it to the relation at the head of the rule

toFact :: Rule -> Fact
toFact (Rule (Relation name vars) _) = Fact name (map valueOf vars)

valueOf :: Variable -> String
valueOf (Value s) = s
valueOf _ = "NOT FOUND"

-- a rule is satisfied if no symbols remain in the head relation
ruleSatisfied :: Rule -> Bool
ruleSatisfied (Rule (Relation _ vars) _) = all isValue vars

isValue :: Variable -> Bool
isValue (Value _) = True
isValue _ = False

factsWithName :: String -> Set Fact -> Set Fact
factsWithName name = Data.Set.filter (factHasName name)

factHasName :: String -> Fact -> Bool
factHasName n (Fact name _) = n == name

-- TODO: this is where the magic happens. need to unify/substitute in values to find facts that work!

main :: IO ()
main = let parsed = parseRunic myProgramText in
  case parsed of
    (Left err) -> print err
    (Right program) -> print (runRunic program)
