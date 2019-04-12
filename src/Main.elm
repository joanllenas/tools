port module Main exposing (Model, Msg(..), init, main, onUrlChange, onUrlRequest, update, view)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation exposing (Key)
import Html exposing (Attribute, Html, a, button, div, h1, h2, hr, img, nav, option, p, pre, section, select, span, strong, text, textarea)
import Html.Attributes exposing (class, cols, href, id, name, rows, selected, src, type_, value)
import Html.Events exposing (on, onClick, onInput, targetValue)
import Json.Decode as Decode
import Url exposing (Url)



---- MODEL ----


type JsonFormatSpacing
    = Spacing0
    | Spacing2
    | Spacing3
    | Spacing4


type NotificationType
    = ErrorNotification
    | SuccessNotification


type alias Notification =
    { message : String
    , type_ : NotificationType
    }


type alias Model =
    { key : Key
    , url : Url
    , jsonToFormat : String
    , jsonFormatted : String
    , notification : Maybe Notification
    , jsonFormatSpacing : JsonFormatSpacing
    }


init : () -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    ( { key = key
      , url = url
      , jsonToFormat = ""
      , jsonFormatted = ""
      , notification = Nothing
      , jsonFormatSpacing = Spacing2
      }
    , Cmd.none
    )



---- PORTS ----


port copyJsonToClipboardRequest : String -> Cmd msg


port copyJsonToClipboardResponse : (Decode.Value -> msg) -> Sub msg


port formatJsonRequest : { json : String, spacing : Int } -> Cmd msg


port formatJsonResponse : (Decode.Value -> msg) -> Sub msg



---- UPDATE ----


type Msg
    = NoOp
    | FormatJsonButtonClick
    | JsonInput String
    | JsonFormatted (Result String String)
    | SetNotification (Maybe Notification)
    | SpacingChange JsonFormatSpacing
    | CopyJsonToClipboard


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FormatJsonButtonClick ->
            let
                formatOptions =
                    { json = model.jsonToFormat
                    , spacing =
                        case model.jsonFormatSpacing of
                            Spacing0 ->
                                0

                            Spacing2 ->
                                2

                            Spacing3 ->
                                3

                            Spacing4 ->
                                4
                    }
            in
            ( model, formatJsonRequest formatOptions )

        JsonInput jsonToFormat ->
            ( { model | jsonToFormat = jsonToFormat }, Cmd.none )

        JsonFormatted result ->
            case result of
                Ok res ->
                    ( { model | jsonFormatted = res, notification = Nothing }, Cmd.none )

                Err err ->
                    ( { model | jsonFormatted = "", notification = Just { message = err, type_ = ErrorNotification } }, Cmd.none )

        SetNotification notification_ ->
            ( { model | notification = notification_ }, Cmd.none )

        SpacingChange spacing ->
            ( { model | jsonFormatSpacing = spacing }, Cmd.none )

        CopyJsonToClipboard ->
            ( model, copyJsonToClipboardRequest "copiable" )

        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ formatJsonResponse (decodeJsonFormatResult >> extractResult >> JsonFormatted)
        , copyJsonToClipboardResponse (decodeCopyJsonToClipboardResponse >> toNotification >> Just >> SetNotification)
        ]


extractResult : Result Decode.Error (Result String String) -> Result String String
extractResult decoded =
    case decoded of
        Ok ok ->
            ok

        Err err ->
            Err (Decode.errorToString err)


toNotification : Result Decode.Error String -> Notification
toNotification decoded =
    case decoded of
        Ok str ->
            { type_ = SuccessNotification, message = str }

        Err err ->
            { type_ = ErrorNotification, message = Decode.errorToString err }


decodeJsonFormatResult : Decode.Value -> Result Decode.Error (Result String String)
decodeJsonFormatResult =
    Decode.decodeValue
        (Decode.oneOf
            [ Decode.field "data" Decode.string |> Decode.map Ok
            , Decode.field "error" Decode.string |> Decode.map Err
            ]
        )


decodeCopyJsonToClipboardResponse : Decode.Value -> Result Decode.Error String
decodeCopyJsonToClipboardResponse =
    Decode.decodeValue Decode.string



---- VIEW ----


notificationColor : NotificationType -> String
notificationColor notificationType =
    if notificationType == ErrorNotification then
        "is-danger"

    else
        "is-success"


notification : Maybe Notification -> Html Msg
notification notification_ =
    case notification_ of
        Nothing ->
            div [] []

        Just { type_, message } ->
            div
                [ class ("notification " ++ notificationColor type_)
                ]
                [ button [ class "delete", onClick (SetNotification Nothing) ]
                    []
                , text message
                ]


navBar =
    section [ class "hero is-dark" ]
        [ div [ class "hero-body" ]
            [ div [ class "container" ]
                [ h1 [ class "title" ]
                    [ text "Json tools 🛠" ]
                , h2 [ class "subtitle" ]
                    [ text "format | parse" ]
                ]
            ]
        ]


onChange : (String -> msg) -> Attribute msg
onChange onChangeAction =
    on "change" <| Decode.map onChangeAction targetValue


stringToSpacing : String -> JsonFormatSpacing
stringToSpacing str =
    case str of
        "4-space" ->
            Spacing4

        "3-space" ->
            Spacing3

        "2-space" ->
            Spacing2

        "0-space" ->
            Spacing0

        _ ->
            Spacing2


jsonInputNav : JsonFormatSpacing -> Html Msg
jsonInputNav spacing =
    nav [ class "level" ]
        [ div [ class "level-left" ]
            [ div [ class "level-item" ]
                [ p [ class "subtitle" ]
                    [ text "Json Input" ]
                ]
            ]
        , div [ class "level-right" ]
            [ div [ class "level-item" ]
                [ div [ class "field has-addons" ]
                    [ div [ class "control is-expanded" ]
                        [ div [ class "select is-fullwidth" ]
                            [ select [ name "country", onChange (stringToSpacing >> SpacingChange) ]
                                [ option [ value "4-space", selected (spacing == Spacing4) ]
                                    [ text "4 Space Tab" ]
                                , option [ value "3-space", selected (spacing == Spacing3) ]
                                    [ text "3 Space Tab" ]
                                , option [ value "2-space", selected (spacing == Spacing2) ]
                                    [ text "2 Space Tab" ]
                                , option [ value "0-space", selected (spacing == Spacing0) ]
                                    [ text "Compact" ]
                                ]
                            ]
                        ]
                    , div [ class "control" ]
                        [ button [ class "button is-outlined is-primary", type_ "button", onClick FormatJsonButtonClick ]
                            [ text "Format" ]
                        ]
                    ]
                ]
            ]
        ]


jsonOutputNav =
    nav [ class "level" ]
        [ div [ class "level-left" ]
            [ div [ class "level-item" ]
                [ p [ class "subtitle" ]
                    [ text "Json Output" ]
                ]
            ]
        , div [ class "level-right" ]
            [ div [ class "level-item" ]
                [ a [ class "button is-outlined is-primary", onClick CopyJsonToClipboard ]
                    [ text "Copy" ]
                ]
            ]
        ]


view : Model -> Document Msg
view model =
    { title = "Json tools"
    , body =
        [ navBar
        , notification model.notification
        , section [ class "section" ]
            [ div [ class "tile is-ancestor" ]
                [ div [ class "tile is-half is-vertical is-parent" ]
                    [ div [ class "tile is-child box" ]
                        [ jsonInputNav model.jsonFormatSpacing
                        , textarea [ class "textarea", rows 18, onInput JsonInput, value model.jsonToFormat ] []
                        ]
                    ]
                , div [ class "tile is-parent" ]
                    [ div [ class "tile is-child box" ]
                        [ jsonOutputNav
                        , textarea [ id "copiable", class "textarea", rows 18, value model.jsonFormatted ] []
                        ]
                    ]
                ]
            ]
        ]
    }



---- URL HANDLING    ----


onUrlRequest : UrlRequest -> Msg
onUrlRequest urlRequest =
    NoOp


onUrlChange : Url -> Msg
onUrlChange url =
    NoOp



---- PROGRAM ----


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }
