module Main exposing (..)

import Debug exposing (log)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (..)


main =
    program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { cryptoName : Maybe String
    , cryptoAmount : Maybe Float
    , fiat : Maybe String
    , cryptoPrice : Maybe Float
    , output : String
    }


init : ( Model, Cmd Msg )
init =
    ( { cryptoName = Nothing
      , cryptoAmount = Nothing
      , fiat = Nothing
      , cryptoPrice = Nothing
      , output = ""
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = ChangeCryptoName String
    | ChangeCryptoAmount String
    | ChangeFiat String
    | GetCryptoPrice
    | NewPrice (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeCryptoName newName ->
            ( { model | cryptoName = Just newName }, Cmd.none )

        ChangeCryptoAmount newAmount ->
            ( { model | cryptoAmount = stringToMaybeFloat newAmount }, Cmd.none )

        ChangeFiat newFiat ->
            ( { model | fiat = Just newFiat }, Cmd.none )

        GetCryptoPrice ->
            ( model, getCryptoPrice model.cryptoName (stringOrEmpty model.fiat) )

        NewPrice (Ok newPrice) ->
            ( { model | cryptoPrice = stringToMaybeFloat newPrice }, Cmd.none )

        NewPrice (Err e) ->
            let
                a =
                    log "error" e
            in
            ( model, Cmd.none )


stringOrEmpty : Maybe String -> String
stringOrEmpty str =
    case str of
        Just s ->
            s

        Nothing ->
            ""


stringToMaybeFloat : String -> Maybe Float
stringToMaybeFloat str =
    let
        result =
            String.toFloat str
    in
    case result of
        Ok f ->
            Just f

        Err _ ->
            Nothing


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


getCryptoPrice : Maybe String -> String -> Cmd Msg
getCryptoPrice crypto fiat =
    case crypto of
        Nothing ->
            Cmd.none

        Just crypto ->
            let
                url =
                    "https://api.coinmarketcap.com/v1/ticker/"
                        ++ crypto
                        ++ "/?convert="
                        ++ fiat

                request =
                    Http.get url (decodeCryptoPrice fiat)
            in
            Http.send NewPrice request


decodeCryptoPrice : String -> Decoder String
decodeCryptoPrice fiat =
    index 0 (field ("price_" ++ String.toLower fiat) string)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ inputRow "Crypto name:" "e.g. bitcoin, ripple..." ChangeCryptoName
        , inputRow "Crypto amount:" "Enter amount..." ChangeCryptoAmount
        , inputRow "Convert to fiat:" "e.g. USD, EUR..." ChangeFiat
        , div [] [ text model.output ]
        ]


inputRow : String -> String -> (String -> Msg) -> Html Msg
inputRow label placeholderText msg =
    div []
        [ span [] [ text label ]
        , input
            [ placeholder placeholderText, onInput msg ]
            []
        ]



{-
   view model =
       div []
           [ input
               [ placeholder "Enter amount..."
               , onInput ChangeInputText
               ]
               []
           , button [ onClick ChangeOutputText ] [ text "Change amount" ]
           , div [] [ text model.outputText ]
           , button [ onClick GetCryptoPrice ] [ text ("Get " ++ model.cryptoName ++ " price") ]
           , div [] [ text model.cryptoPrice ]
           ]
-}
