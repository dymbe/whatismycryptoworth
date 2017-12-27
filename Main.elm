module Main exposing (..)

import Debug exposing (log)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


type alias Model =
    { inputText : String
    , outputText : String
    }


model : Model
model =
    { inputText = ""
    , outputText = ""
    }


type Msg
    = ChangeInputText String
    | ChangeOutputText


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeInputText newInput ->
            log "InputChange"
                { model | inputText = newInput }

        ChangeOutputText ->
            log "ChangeOutputText"
                { model | outputText = getOutputText model.inputText }


toInt : String -> Int
toInt str =
    let
        result =
            String.toInt str
    in
    case result of
        Ok v ->
            v

        Err e ->
            0


getOutputText : String -> String
getOutputText inputText =
    let
        result =
            String.toInt inputText
    in
    case result of
        Ok i ->
            "You have " ++ toString i ++ " something!"

        Err e ->
            "Invalid input!"


view : Model -> Html Msg
view model =
    div []
        [ input
            [ placeholder "Enter amount..."
            , onInput ChangeInputText
            ]
            []
        , button [ onClick ChangeOutputText ] [ text "Change amount" ]
        , div [] [ text model.outputText ]
        ]


main =
    beginnerProgram
        { model = model
        , view = view
        , update = update
        }
