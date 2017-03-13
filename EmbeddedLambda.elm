port module EmbeddedLambda exposing (main)

import Html exposing (Html, text)
import Html.Attributes exposing (style)
import Html.Events
import Lambda exposing (Term)
import LambdaLang exposing (EvalResult)
import Array exposing (Array)

import UtilityFunctions as Util

port evaluate : (String -> msg) -> Sub msg


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> evaluate ProgramChanged
        }



type alias Model =
    { program : String
    , result : Result String (List EvalResult)
    }

type Msg = ProgramChanged String | SetOpen Bool Int

noCmd : Model -> (Model, Cmd Msg)
noCmd m =
    (m, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ProgramChanged newProg ->
      Model newProg (LambdaLang.runProgram newProg) |> noCmd
    SetOpen state id ->
      let
        newResult = model.result
            |> Result.map (
                  Array.fromList
                  >> Util.updateArr id (\res -> {res | openState = state})
                  >> Array.toList
               )
      in
        {model | result = newResult} |> noCmd

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

{-
viewStepwise : List Term -> List (Html Msg)
viewStepwise terms =
    case terms of
        []      -> []
        [x]     -> [Html.h3 [ma 5] [text <| Lambda.showTerm x]]
        (x::xs) -> Html.h3 [ma 5] [text <| Lambda.showTerm x] :: viewStepwise xs
-}

viewContractable : Int -> Bool -> Html Msg -> Html Msg
viewContractable id open content = content

viewEvaluation : Int -> EvalResult -> List (Html Msg)
viewEvaluation id res =
    let
        title = Html.h3 [ma 5] [text <| res.code]
        evalLines = List.map (\x -> Html.h3 [lightGrey, ma 5] [text <| Lambda.showTerm x]) res.evaluation
        resultLine = Html.h3 [ma 5] [text <| Lambda.showTerm res.result]
    in
        [ title
        , viewContractable id res.openState <| Html.div [] evalLines
        , resultLine
        ]



view : Model -> Html Msg
view model =
    Html.div [Html.Attributes.style [("text-align", "center")]]
        [ case model.result of
            Err error ->
                Html.h3[] [text error]
            Ok termChains ->
                List.indexedMap viewEvaluation termChains
                |> List.map (\x -> Html.div [mb 30] x)
                |> Html.div []
        ]