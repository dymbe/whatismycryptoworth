port module Main exposing (..)

import Debug exposing (log)
import FormatNumber exposing (format)
import FormatNumber.Locales as Locales
import Html exposing (..)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Json
import Json.Decode.Pipeline exposing (decode, optional, required)
import String exposing (isEmpty)


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { nameInput : String
    , fiatInput : String
    , amountInput : String
    , cryptoName : Maybe String
    , cryptoSymbol : Maybe String
    , fiat : Maybe String
    , amount : Float
    , valueUSD : Maybe Float
    , valueFiat : Maybe Float
    }


init : ( Model, Cmd Msg )
init =
    ( { nameInput = ""
      , fiatInput = ""
      , amountInput = ""
      , cryptoName = Nothing
      , cryptoSymbol = Nothing
      , fiat = Nothing
      , amount = 0
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
    | Save
    | Load
    | Receive String


type alias Response =
    { name : String
    , symbol : String
    , valueUSD : String
    , valueFiat : Maybe String
    }


port save : String -> Cmd Msg


port load : () -> Cmd Msg


port receive : (String -> Msg) -> Sub Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeCryptoName newName ->
            ( { model | nameInput = newName }, Cmd.none )

        ChangeCryptoAmount newAmount ->
            ( { model | amountInput = newAmount }, Cmd.none )

        ChangeFiat newFiat ->
            ( { model | fiatInput = newFiat }, Cmd.none )

        GetCryptoPrice ->
            ( { model
                | fiat = Just model.fiatInput
                , amount = stringToFloatOrZero model.amountInput
              }
            , getCryptoPrice model.nameInput model.fiatInput
            )

        NewPrice (Ok response) ->
            ( { model
                | cryptoName = Just response.name
                , cryptoSymbol = Just response.symbol
                , valueUSD =
                    response.valueUSD
                        |> stringToMaybeFloat
                , valueFiat =
                    response.valueFiat
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
                | cryptoName = Nothing
                , cryptoSymbol = Nothing
                , valueUSD = Nothing
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


stringToFloatOrZero : String -> Float
stringToFloatOrZero string =
    let
        result =
            String.toFloat string
    in
    case result of
        Ok f ->
            f

        Err _ ->
            0


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
    if crypto |> isEmpty |> not then
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
    else
        Cmd.none


ddecodeCryptoPrice : String -> Json.Decoder String
ddecodeCryptoPrice fiat =
    Json.index 0 (Json.field ("price_" ++ String.toLower fiat) Json.string)


decodeCryptoPrice : String -> Json.Decoder Response
decodeCryptoPrice fiat =
    Json.index 0
        (decode Response
            |> required "name" Json.string
            |> required "symbol" Json.string
            |> required "price_usd" Json.string
            |> optional ("price_" ++ String.toLower fiat) (Json.map Just Json.string) Nothing
        )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    load Load



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ inputRow "Crypto name:" "e.g. bitcoin, ripple..." ChangeCryptoName
        , inputRow "Crypto amount:" "Enter amount..." ChangeCryptoAmount
        , inputRow "Preferred fiat:" "e.g. USD, EUR..." ChangeFiat
        , button [ onClick GetCryptoPrice ] [ text "Get value" ]
        , resultRow model
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


resultRow : Model -> Html Msg
resultRow model =
    case model.valueUSD of
        Just valueUSD ->
            let
                ( valueFiat, fiat ) =
                    case model.valueFiat of
                        Just float ->
                            ( float, stringOrEmpty model.fiat )

                        Nothing ->
                            ( valueUSD, "USD" )
            in
            div []
                [ div []
                    [ h1 [] [ text (stringOrEmpty model.cryptoName) ]
                    , h2 [] [ text ("$" ++ format Locales.usLocale valueUSD) ]
                    ]
                , div []
                    [ text
                        (format Locales.usLocale model.amount
                            ++ " "
                            ++ stringOrEmpty model.cryptoSymbol
                            ++ " = "
                            ++ format Locales.usLocale (model.amount * valueFiat)
                            ++ " "
                            ++ String.toUpper fiat
                        )
                    ]
                ]

        Nothing ->
            div [] [ text "No valid cryptocurrency specified :/" ]


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
