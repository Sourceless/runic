module Eval.Naive (eval) where
import Data.Set (Set, empty, unions, fromList, union, filter, singleton)
import qualified Data.Set
import AST (Program(..), Rule(..), Relation(..), Variable(..), Assertion(..))
import Data.Maybe (isJust, fromJust)
import Data.Map (Map, lookup, fromList)

data Fact = Fact String [String] deriving (Show, Eq, Ord)

eval :: Program -> Set Fact
eval p = eval' p empty

eval' :: Program -> Set Fact -> Set Fact
eval' program facts =
  let (program', facts') = evalStep program facts in
        if facts == facts'
        then facts'
        else eval' program facts'
-- TODO optimisation below where the program is thrown away each iteration and replaced

evalStep :: Program -> Set Fact -> (Program, Set Fact)
evalStep (Program rules) facts =
  let (fs, rs) = unzip (map (evalRule facts) rules)
      facts' = unions fs
      rules' = unions rs
  in
    (Program (Data.Set.toList rules'), facts')

-- using facts we know and an existing rule, derive new facts
evalRule :: Set Fact -> Rule -> (Set Fact, Set Rule)
evalRule facts (Rule (Relation name vars) []) =
  -- if we are out of relations in the body, if all the vars have unified, create a new fact
  let rule = Rule (Relation name vars) [] in
        if ruleSatisfied rule
        then (facts `union` singleton (toFact rule), empty)
        else (facts, empty)
evalRule facts rule =
  -- go through each fact we have and try to apply it to the relation at the head of the rule
  let rules' = map (substitute rule) (Data.Set.toList facts)
      boundRules = Prelude.filter isJust rules'
      -- run them through again so we reach the base case
      boundRules' = map (evalRule facts . fromJust) boundRules

      -- take the newly descovered facts, roll them up, and add them to our existing knowledge
      (newFacts, newRules) = unzip boundRules'
      newFacts' = unions (facts : newFacts)
      newRules' = unions newRules
  in
    (newFacts', newRules')

fromAssertion :: Assertion -> Relation
fromAssertion (Id r) = r
fromAssertion (Not r) = r

isId :: Assertion -> Bool
isId (Id _) = True
isId _ = False

substitute :: Rule -> Fact -> Maybe Rule
substitute (Rule (Relation name vars) (a:as)) (Fact fname fvars) =
  -- see if the fact can be applied to the head relation in the rule body
  let rule = Rule (Relation name vars) (a:as)
      fact = Fact fname fvars
      (Relation rname rvars) = fromAssertion a
  in
    -- the rule name and number of vars must be the same!
    if fname == rname && length fvars == length rvars
    then
      -- for each param -- if it is a symbol, bind it, otherwise ensure it matches
        let bindings = zipWith bind rvars fvars in
                -- if everything bound, great! let's produce a new rule by unifying!
                -- otherwise, we can't go any further, so drop it
                if isId a
                   then if all isJust bindings
                        then Just (unify rule fact)
                        else Nothing
                   else if all isJust bindings
                        then Nothing
                        else Just (unify rule fact)
    else Nothing

bind :: Variable -> String -> Maybe String
bind (Value a) b =
  if a == b
  then Just a
  else Nothing
bind (Symbol a) b = Just b

setRelation :: Assertion -> Relation -> Assertion
setRelation (Id _) r = Id r
setRelation (Not _) r = Not r

unify :: Rule -> Fact -> Rule
-- takes a rule and binds any symbols in first body relation everywhere in the rule: the head, and any other body relations
unify (Rule (Relation name vars) (a:as)) (Fact fname fvars) =
  -- first we need to get a mapping from symbol names to concrete values
  let
      (Relation rname rvars) = fromAssertion a
      bindings = Data.Map.fromList (zip rvars fvars)
      newHead = unifyRelation (Id (Relation name vars)) bindings
      newBody = map (flip unifyRelation bindings) as
  in
    Rule (fromAssertion newHead) newBody

unifyRelation :: Assertion -> Map Variable String -> Assertion
unifyRelation a symbolMap =
  let (Relation name vars) = fromAssertion a
      r = Relation name (map (unifySingle symbolMap) vars)
  in
    setRelation a r

unifySingle :: Map Variable String -> Variable -> Variable
unifySingle bindings (Symbol name) =
  let value = Data.Map.lookup (Symbol name) bindings
  in
    case value of
      Just v -> (Value v)
      Nothing -> (Symbol name)

unifySingle _ v = v

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
