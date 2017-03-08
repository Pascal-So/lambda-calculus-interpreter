import Html exposing (Html, text)
import Html.Attributes exposing (style)
import Html.Events
import Set exposing (Set)
import Char
import Dict exposing (Dict)

import Debug exposing (log)

import Parser exposing ((<*), (*>), (+++), (>>=), Parser)
import Lambda exposing (Term)
import LambdaParser

--------------------------- MAIN ------------------------------------------

main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
        -- text <| toString <| allTestsOk
        -- text <| showFailedTests
        -- text <| toString <| testsOk testParsing


-- (\123.2(123))(\12.2)

type alias Model =
    { program : String
    , result : Result String (List (List Term))
    }

type Msg = ProgramChanged String

noCmd : Model -> (Model, Cmd Msg)
noCmd m =
    (m, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ProgramChanged newProg ->
            Model newProg (runProgram newProg) |> noCmd

init : (Model, Cmd Msg)
init =
    Model "" (Err "Empty program") |> noCmd


lightGrey : Html.Attribute a
lightGrey = style [("color", "#777")]

darkGrey : Html.Attribute a
darkGrey = style [("color", "#222")]

ma : Int -> Html.Attribute a
ma x = style [("margin", toString x ++ "px")]

mb : Int -> Html.Attribute a
mb x = style [("margin-bottom", toString x ++ "px")]

viewStepwise : List Term -> List (Html Msg)
viewStepwise terms =
    case terms of
        []      -> []
        [x]     -> [Html.h3 [darkGrey, ma 5] [text <| Lambda.showTerm x]]
        (x::xs) -> Html.h3 [lightGrey, ma 5] [text <| Lambda.showTerm x] :: viewStepwise xs


view : Model -> Html Msg
view model =
    Html.div [Html.Attributes.style [("text-align", "center")]]
        [ Html.textarea
            [ Html.Attributes.cols 60
            , Html.Attributes.rows 7
            , Html.Attributes.value model.program
            , Html.Events.onInput ProgramChanged]
            []
        , case model.result of
            Err error ->
                Html.h3[] [text error]
            Ok termChains ->
                List.map viewStepwise termChains
                |> List.map (\x -> Html.div [mb 30] x)
                |> Html.div []
        ]


{-
runProgram : String -> Result String (List (List Term))
runProgram str =
    (parseProgram <* Parser.eof) str
    |> log "parse"
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
    |> Result.map (List.map Lambda.evaluateStepwise << programToExpressions)

-}

runParser : Parser a -> String -> Result String a
runParser p str =
    p str
    |> List.head
    |> Result.fromMaybe "Could not parse Program"
    |> Result.andThen (\(result, rest) ->
        if String.isEmpty rest then
            Ok result
        else
            Err "Not entire line parsed"
    )

numberItems : List a -> List (Int, a)
numberItems = List.indexedMap (\id a -> (id+1, a))

sequenceResults : List (Result x a) -> Result x (List a)
sequenceResults = List.foldr (Result.map2 (::)) (Ok [])


runProgram : String -> Result String (List (List Term))
runProgram str =
    String.lines str
    
    |> List.map String.trim
    |> numberItems
    
    |> List.filter (not << String.isEmpty << Tuple.second)
    |> List.map (Tuple.mapSecond (runParser parseProgramLine))
    |> List.map (\(id, a) -> Result.mapError (\error -> toString id ++ ": " ++ error) a)
    
    |> sequenceResults
    |> log "pt a"
    |> Result.andThen (\program ->
         if hasDuplicateVars program then
             Err "Some variables have been assigned twice"
         else
             Ok program
       )
    |> Result.map (List.map Lambda.evaluateStepwise << programToExpressions)


----------------- Dependency Graph ------------------------------

indexList : List a -> List (Int, a)
indexList = List.indexedMap (,)

setFilterMap : (comparable -> Maybe comparable1) -> Set comparable -> Set comparable1
setFilterMap f = Set.fromList << List.filterMap f << Set.toList


getAssignmentsIdDict : List ProgramLine -> Dict VarName Int
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
    baseGraph = fromVerts lst
    definitions = getAssignmentsIdDict lst
    
  in
    getDependencies lst
    |> List.map (
         setFilterMap (\dep ->
           Dict.get dep definitions
         )
       )
    |> indexList
    |> List.foldr (\(id, deps)-> addEdges id deps) baseGraph



------------- Graph library --------------------------


updateArr : Int -> (a -> a) -> Array a -> Array a
updateArr pos func arr =
  Array.get pos arr
  |> Maybe.map func
  |> Maybe.map (\upd -> Array.set pos upd arr)
  |> Maybe.withDefault arr
  

type alias Graph a = Array (a, Set Int)

addEdge : Int -> Int -> Graph a -> Graph a
addEdge a b =
  updateArr a (Tuple.mapSecond (Set.insert b))

addEdges : Int -> Set Int -> Graph a -> Graph a
addEdges a bs =
  updateArr a (Tuple.mapSecond (Set.union bs))
  

fromVerts : List a -> Graph a
fromVerts lst =
  lst
  |> List.map (\node -> (node, Set.empty))
  |> Array.fromList



------------- Wrapper Language Transformer ---------------

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



------------ Wrapper Language Parser ----------------------


type ProgramLine = Assignment Lambda.VarName Term | Expression Term


parseAssignment : Parser ProgramLine
parseAssignment =
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
