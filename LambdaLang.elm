module LambdaLang exposing
    ( runProgram
    )


import Set exposing (Set)
import Array exposing (Array)
import Dict exposing (Dict)
import Parser exposing ((<*), (*>), (+++), (>>=), Parser)
import Lambda exposing (Term)
import LambdaParser
import Graph exposing (Graph)


numberItems : List a -> List (Int, a)
numberItems = List.indexedMap (\id a -> (id+1, a))

sequenceResults : List (Result x a) -> Result x (List a)
sequenceResults = List.foldr (Result.map2 (::)) (Ok [])


runProgram : String -> Result String (List (List Term))
runProgram str =
  (parseProgram <* Parser.eof) str
    -- |> log "parse"
    |> List.head 
    |> Maybe.map Tuple.first
    |> Result.fromMaybe "Could not parse program"
    |> Result.andThen (\program ->
         if List.isEmpty program then
           Err "Empty program" 
         else
           Ok program
       )
    |> Result.andThen (\program ->
         if hasDuplicateVars program then
           Err "Some variables have been assigned twice"
         else
           Ok program
       )
    |> Result.andThen (\program ->
         let
           graph = getDependencyGraph program
           loops = Graph.hasLoop graph
         in
           if loops then
             Err "The dependency graph contains a loop"
           else
             Ok graph
       )
    |> Result.map evaluateProgramGraph
    |> Result.map (List.map2 (,) (nonemptyLines str))
    |> Result.andThen (List.map ( \(code, terms) ->
           let
               result = last terms
               evaluation = initList terms
           in
               case (result, evaluation) of
                   (Just res, Just ev) ->
                       Ok (code, ev, res, True)
                   _ ->
                       Err "Internal error: Evaluation didn't return a result"
       ) >> sequenceResults )

----------------- Dependency Graph ------------------------------

indexList : List a -> List (Int, a)
indexList = List.indexedMap (,)

setFilterMap : (comparable -> Maybe comparable1) -> Set comparable -> Set comparable1
setFilterMap f = Set.fromList << List.filterMap f << Set.toList


getAssignmentsIdDict : List ProgramLine -> Dict Lambda.VarName Int
getAssignmentsIdDict lst =
  indexList lst
  |> List.filterMap (\(id, line) ->
       case line of 
         Expression _ -> Nothing
         Assignment name term -> Just (name, id)
     )
  |> Dict.fromList


getDependencyGraph : List ProgramLine -> Graph ProgramLine
getDependencyGraph lst = 
  let
    baseGraph = Graph.fromVerts lst
    definitions = getAssignmentsIdDict lst
    
  in
    List.map getDependencies lst
    |> List.map (
         setFilterMap (\dep ->
           Dict.get dep definitions
         )
       )
    |> indexList
    |> List.foldr (\(id, deps)-> Graph.addEdges id deps) baseGraph





------------- Wrapper Language Transformer ---------------

getDependencies : ProgramLine -> Set Lambda.VarName
getDependencies line =
    case line of
        Expression term -> Lambda.getFreeVars term
        Assignment _ term -> Lambda.getFreeVars term


getAssignmentsDict : List ProgramLine -> Dict Lambda.VarName Term
getAssignmentsDict lines =
    lines
    |> List.filterMap (\line ->
         case line of
            Assignment a b -> Just (a, b)
            Expression _   -> Nothing
       )
    |> Dict.fromList


getExpressionTerms : List ProgramLine -> List Term
getExpressionTerms lines =
    lines
    |> List.filterMap (\line ->
         case line of
             Expression term -> Just term
             Assignment _ _  -> Nothing
       )


wrapInLambdas : Dict Lambda.VarName Term -> Term -> Term
wrapInLambdas dict term =
    let
        freeVars = Lambda.getFreeVars term
        termsToAssign = dict
        |> Dict.filter (\var _ ->
            Set.member var freeVars
           )
        |> Dict.toList
    in
        termsToAssign
        |> List.foldr (\(name, val) accTerm ->
            Lambda.App (Lambda.Lambda name accTerm) val
         ) term


-- only run on valid program (no duplicate variable assignments),
-- otherwise duplicate assignments will be overridden
programToExpressions : List ProgramLine -> List Term
programToExpressions lines =
    let
        assignments = getAssignmentsDict lines
        terms = getExpressionTerms lines
    in
        List.map (wrapInLambdas assignments) terms


hasDuplicateVars : List ProgramLine -> Bool
hasDuplicateVars prog =
    let
        vars =
            prog
            |> List.filterMap (\line ->
                case line of
                    Assignment var _ -> Just var
                    Expression _     -> Nothing
               )
        len = List.length vars
        uniqueLen = Set.size (Set.fromList vars)
    in
        len /= uniqueLen

flattenMaybe : Maybe (Maybe a) -> Maybe a
flattenMaybe m =
    case m of
        Nothing -> Nothing
        Just a  -> a


getTerm : Int -> Graph ProgramLine -> Maybe Term
getTerm id graph =
    Graph.getValue id graph
    |> Maybe.map (\val ->
           case val of
               Assignment _ term -> term
               Expression term   -> term
       )

getDependencyName : Int -> Graph ProgramLine -> Maybe Lambda.VarName
getDependencyName id graph =
    Graph.getValue id graph
    |> Maybe.andThen (\val ->
           case val of
               Assignment name _ -> Just name
               Expression _      -> Nothing
       )

transposeTupleMaybe : (Maybe a, Maybe b) -> Maybe (a, b)
transposeTupleMaybe pair =
    case pair of
        (Just a, Just b) -> Just (a, b)
        _                -> Nothing

last : List a -> Maybe a
last = List.head << List.reverse

initList : List a -> Maybe (List a)
initList = Maybe.map List.reverse << List.tail << List.reverse

-- is already cycle free
evaluateProgramGraph : Graph ProgramLine -> List (List Term)
evaluateProgramGraph graph = 
    let
        evalOrder = Graph.topoSort graph
            |> List.reverse
        size = Graph.getSize graph
        evaluated = Array.repeat size Nothing
    in
        List.foldl (\id evaluated ->
            let
                deps = Graph.getOutEdges id graph
                    |> Maybe.withDefault Set.empty
                    |> Set.toList
                    |> List.map (\id -> 
                           Array.get id evaluated
                           |> flattenMaybe
                           |> Maybe.andThen last
                           |> (\val -> (getDependencyName id graph, val))
                           |> transposeTupleMaybe
                       )
                    |> List.filterMap identity
                    |> Dict.fromList
                evaluatedTerm = getTerm id graph
                    |> Maybe.map (Lambda.evaluateStepwise << wrapInLambdas deps)
            in
                Array.set id evaluatedTerm evaluated
        ) evaluated evalOrder
        |> Array.toList
        |> List.filterMap identity
  

------------ Wrapper Language Parser ----------------------


type ProgramLine = Assignment Lambda.VarName Term | Expression Term


parseAssignment : Parser ProgramLine
parseAssignment =
    Parser.string "let" *>
    Parser.whitespace *>
    LambdaParser.parseVarName >>= \varname ->
    Parser.whitespace *>
    Parser.char '=' *>
    Parser.whitespace *>
    Parser.inRestOfLine LambdaParser.parseTerm >>= \term ->
    Parser.return (Assignment varname term)

parseExpression : Parser ProgramLine
parseExpression =
    LambdaParser.parseTerm >>= \term ->
    Parser.return (Expression term)


parseProgramLine : Parser ProgramLine
parseProgramLine =
    Parser.whitespace *>
    parseAssignment +++ parseExpression >>= \line ->
    Parser.whitespace *>
    Parser.return line



parseProgram : Parser (List ProgramLine)
parseProgram =
    Parser.many
        ( Parser.whitespace *>
          (Parser.inRestOfLine (parseExpression <* Parser.whitespace) +++ parseAssignment)
          <* Parser.whitespace) >>= \lines ->
    Parser.return lines



------------ Lib ---------------------------


sequenceResults : List (Result a b) -> Result a (List b)
sequenceResults = 
    Result.map List.reverse << List.foldr (Result.map2 (::)) (Ok []) << List.reverse