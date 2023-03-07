module Parser (Parser.parse) where

import AST
    ( Program(..),
      Variable(..),
      Relation(..),
      Assertion(..),
      Rule(..) )
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
      choice,
      try,
      ParseError,
      GenParser )

program :: GenParser Char st Program
program =
    do result <- many rule
       eof
       return (Program result)

rule :: GenParser Char st Rule
rule =
    do spaces
       head <- relation
       spaces
       body <- ruleBody
       spaces
       char '.'
       spaces
       return (Rule head body)

ruleBody :: GenParser Char st [Assertion]
ruleBody =
    option [] (do
       implies
       sepBy assertion ruleBodySeperator
    )

ruleBodySeperator :: GenParser Char st ()
ruleBodySeperator =
  do spaces
     char ','
     spaces
     return ()

implies :: GenParser Char st ()
implies = do
    spaces
    string "<-"
    spaces
    return ()

assertion :: GenParser Char st Assertion
assertion =
  choice [ try notAssertion, try succeedAssertion, try failAssertion, idAssertion ]

succeedAssertion :: GenParser Char st Assertion
succeedAssertion = do
  string "succeed"
  spaces
  return Succeed

failAssertion :: GenParser Char st Assertion
failAssertion = do
  string "fail"
  spaces
  return Fail

notAssertion :: GenParser Char st Assertion
notAssertion = do
  string "not"
  spaces
  r <- relation
  return (Not r)

idAssertion :: GenParser Char st Assertion
idAssertion = do
  r <- relation
  return (Id r)

getValue :: Variable -> String
getValue (Symbol a) = a
getValue (Value a) = a

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

parse :: String -> Either ParseError Program
parse = Text.ParserCombinators.Parsec.parse program "(unknown)"
