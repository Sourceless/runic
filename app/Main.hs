{-# LANGUAGE InstanceSigs #-}
module Main where
import Data.List (intercalate)
import Text.ParserCombinators.Parsec
    ( char,
      letter,
      noneOf,
      spaces,
      string,
      between,
      eof,
      option,
      sepBy,
      (<|>),
      many,
      parse,
      ParseError,
      GenParser )
import Data.Set (Set, empty, unions, fromList, union, filter, singleton)
import qualified Data.Set
import Data.Maybe (isJust)
import Data.Map (Map, lookup, fromList)

newtype Program = Program [Rule]
data Rule = Rule Relation [Relation] deriving (Eq, Ord)
data Relation = Relation Name [Variable] deriving (Eq, Ord)
type Name = String
data Variable = Symbol String | Value String deriving (Eq, Ord)

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
    var <- rValue <|> rSymbol
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
                    , Rule (Relation "reachable" [Symbol "x", Symbol "x"]) [Relation "station" [Symbol "x"]]]

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
runRunic' program facts =
  let (program', facts') = runRunicStep program facts in
        if facts == facts'
        then facts'
        else runRunic' program facts'
-- TODO optimisation below where the program is thrown away each iteration and replaced

runRunicStep :: Program -> Set Fact -> (Program, Set Fact)
runRunicStep (Program rules) facts =
  let (fs, rs) = unzip (map (runRunicRule facts) rules)
      facts' = unions fs
      rules' = unions rs
  in
    (Program (Data.Set.toList rules'), facts')

-- using facts we know and an existing rule, derive new facts
runRunicRule :: Set Fact -> Rule -> (Set Fact, Set Rule)
runRunicRule facts (Rule (Relation name vars) []) =
  -- if we are out of relations in the body, if all the vars have unified, create a new fact
  let rule = Rule (Relation name vars) [] in
        if ruleSatisfied rule
        then (facts `union` singleton (toFact rule), empty)
        else (facts, empty)
runRunicRule facts rule =
  let rules' = map (substitute rule) (Data.Set.toList facts)
      boundRules = Prelude.filter isJust rules'
  in
    _ -- TODO return any new facts and rules found
  -- go through each fact we have and try to apply it to the relation at the head of the rule

substitute :: Rule -> Fact -> Maybe Rule
substitute (Rule (Relation name vars) ((Relation rname rvars):rs)) (Fact fname fvars) =
  -- see if the fact can be applied to the head relation in the rule body
  let rule = Rule (Relation name vars) (Relation rname rvars:rs)
      fact = Fact fname fvars
  in
    -- the rule name and number of vars must be the same!
    if name == rname && length vars == length rvars
    then
      -- for each param -- if it is a symbol, bind it, otherwise ensure it matches
        let bindings = zipWith bind rvars fvars in
                -- if everything bound, great! let's produce a new rule by unifying!
                -- otherwise, we can't go any further, so drop it
                if all isJust bindings
                then Just (unify rule fact)
                else Nothing
    else Nothing

bind :: Variable -> String -> Maybe String
bind (Value a) b =
  if a == b
  then Just a
  else Nothing
bind (Symbol a) b = Just b

unify :: Rule -> Fact -> Rule
-- takes a rule and binds any symbols in first body relation everywhere in the rule: the head, and any other body relations
unify (Rule (Relation name vars) body) (Fact fname fvars) =
  -- first we need to get a mapping from symbol names to concrete values
  let bindings = Data.Map.fromList (zip vars fvars)
  in
    _ -- TODO run unifyRelation over all relations to construct a new rule except the first rule in the body is missing

unifyRelation :: Relation -> Map String String -> Relation
unifyRelation (Relation name vars) symbolMap =
  _ -- TODO replace symbols with values

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
