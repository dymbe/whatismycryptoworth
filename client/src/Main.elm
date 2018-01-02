module Main exposing (..)

import Debug exposing (log)
import Html exposing (..)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Json
import Json.Decode.Pipeline exposing (decode, optional, required)
import String exposing (isEmpty)


main =
    program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { typeInput : String
    , fiatInput : String
    , amountInput : String
    , fiat : Maybe String
    , amount : Maybe Float
    , valueUSD : Maybe Float
    , valueFiat : Maybe Float
    }


init : ( Model, Cmd Msg )
init =
    ( { typeInput = ""
      , fiatInput = ""
      , amountInput = ""
      , fiat = Nothing
      , amount = Nothing
      , valueUSD = Nothing
      , valueFiat = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = ChangeCryptoName String
    | ChangeCryptoAmount String
    | ChangeFiat String
    | GetCryptoPrice
    | NewPrice (Result Http.Error Response)


type alias Response =
    { valueUSD : String
    , valueFiat : Maybe String
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeCryptoName newName ->
            ( { model | typeInput = newName }, Cmd.none )

        ChangeCryptoAmount newAmount ->
            ( { model | amountInput = newAmount }, Cmd.none )

        ChangeFiat newFiat ->
            ( { model | fiatInput = newFiat }, Cmd.none )

        GetCryptoPrice ->
            ( model, getCryptoPrice model.typeInput model.fiatInput )

        NewPrice (Ok newPrice) ->
            ( { model
                | valueUSD =
                    newPrice.valueUSD
                        |> stringToMaybeFloat
                , valueFiat =
                    newPrice.valueFiat
                        |> stringOrEmpty
                        |> stringToMaybeFloat
              }
            , Cmd.none
            )

        NewPrice (Err e) ->
            let
                a =
                    log "error" e
            in
            ( { model
                | valueUSD = Nothing
                , valueFiat = Nothing
              }
            , Cmd.none
            )


emptyIsNothing : String -> Maybe String
emptyIsNothing string =
    if string |> isEmpty then
        Nothing
    else
        Just string


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


zeroFloatIfNothing : Maybe Float -> Float
zeroFloatIfNothing float =
    case float of
        Just f ->
            f

        Nothing ->
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


getCryptoPrice : String -> String -> Cmd Msg
getCryptoPrice crypto fiat =
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


ddecodeCryptoPrice : String -> Json.Decoder String
ddecodeCryptoPrice fiat =
    Json.index 0 (Json.field ("price_" ++ String.toLower fiat) Json.string)


decodeCryptoPrice : String -> Json.Decoder Response
decodeCryptoPrice fiat =
    Json.index 0
        (decode Response
            |> required "price_usd" Json.string
            |> optional ("price_" ++ String.toLower fiat) (Json.map Just Json.string) Nothing
        )



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
        , button [ onClick GetCryptoPrice ] [ text "Get crypto price" ]
        , outputRow "Crypto name:" model.typeInput
        , outputRow "Crypto amount:" model.amountInput
        , outputRow "Convert to fiat:" model.fiatInput
        , outputRow
            "USD"
            (model.valueUSD
                |> zeroFloatIfNothing
                |> toString
            )
        , outputRow
            "Fiat"
            (model.valueFiat
                |> zeroFloatIfNothing
                |> toString
            )
        ]


inputRow : String -> String -> (String -> Msg) -> Html Msg
inputRow label placeholderText msg =
    div []
        [ span [] [ text label ]
        , input
            [ placeholder placeholderText, onInput msg ]
            []
        ]


outputRow : String -> String -> Html Msg
outputRow label output =
    div []
        [ span [] [ text label ]
        , span [] [ text output ]
        ]



{-
   resultRow : Model -> Html Msg
   resultRow model =
       div [] []
-}


stringOrError : Maybe String -> String -> String
stringOrError string error =
    case string of
        Just string ->
            string

        Nothing ->
            error


maybeFloatToMaybeString : Maybe Float -> Maybe String
maybeFloatToMaybeString float =
    case float of
        Just float ->
            Just (toString float)

        Nothing ->
            Nothing


floatToStringOrError : Maybe Float -> String -> String
floatToStringOrError float error =
    case float of
        Just float ->
            toString float

        Nothing ->
            error
