port module EmbeddedLambda exposing (main)

import Html exposing (Html, text)
import Html.Attributes exposing (style)
import Html.Events
import Lambda exposing (Term)
import LambdaLang



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
            Model newProg (LambdaLang.runProgram newProg) |> noCmd

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
        [x]     -> [Html.h3 [ma 5] [text <| Lambda.showTerm x]]
        (x::xs) -> Html.h3 [ma 5] [text <| Lambda.showTerm x] :: viewStepwise xs


view : Model -> Html Msg
view model =
    Html.div [Html.Attributes.style [("text-align", "center")]]
        [ case model.result of
            Err error ->
                Html.h3[] [text error]
            Ok termChains ->
                List.map viewStepwise termChains
                |> List.map (\x -> Html.div [mb 30] x)
                |> Html.div []
        ]