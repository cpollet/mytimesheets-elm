module Main exposing (main)

import Browser
import Html exposing (Attribute, Html, button, div, input, table, tbody, td, text, tfoot, th, thead, tr)
import Html.Attributes exposing (colspan, placeholder, value)
import Html.Events exposing (on, targetValue)
import Json.Decode as Json
import String



-- MAIN


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { entries : List Entry
    }


type alias Entry =
    { from : Time
    , comment : String
    }


type alias Time =
    { hour : Int
    , minute : Float
    }


init : Model
init =
    { entries = []
    }



-- UPDATE


type Msg
    = ValidateTime (Maybe Int) String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ValidateTime rowIndex value ->
            case toTime value of
                Nothing ->
                    model

                Just t ->
                    let
                        item =
                            { from = t, comment = "" }
                    in
                    case rowIndex of
                        Nothing ->
                            { entries = List.append model.entries [ item ] }

                        Just i ->
                            { entries =
                                List.take i model.entries
                                    ++ [ item ]
                                    ++ List.drop (i + 1) model.entries
                            }


toTime : String -> Maybe Time
toTime str =
    if not (String.contains ":" str) then
        case toHour str of
            Nothing ->
                case String.length str of
                    3 ->
                        toTime (String.left 1 str ++ ":" ++ String.dropLeft 1 str)

                    4 ->
                        toTime (String.left 2 str ++ ":" ++ String.dropLeft 2 str)

                    _ ->
                        Nothing

            Just hour ->
                Just { hour = hour, minute = 0 }

    else
        case String.split ":" str of
            [] ->
                Nothing

            h :: [] ->
                case toHour h of
                    Just hour ->
                        Just { hour = hour, minute = 0 }

                    Nothing ->
                        Nothing

            h :: m :: [] ->
                case toHour h of
                    Just hour ->
                        case m of
                            "" ->
                                Just { hour = hour, minute = 0 }

                            _ ->
                                case toMinute m of
                                    Just minute ->
                                        Just { hour = hour, minute = toFloat minute / 60 }

                                    Nothing ->
                                        Nothing

                    Nothing ->
                        Nothing

            _ :: _ :: _ :: _ ->
                Nothing


toHour : String -> Maybe Int
toHour string =
    case String.toInt string of
        Just i ->
            if i < 0 || i > 23 then
                Nothing

            else
                Just i

        Nothing ->
            Nothing


toMinute : String -> Maybe Int
toMinute string =
    case String.toInt string of
        Just i ->
            if i < 0 || i > 59 then
                Nothing

            else
                Just i

        Nothing ->
            Nothing


toDecimal : Time -> Float
toDecimal time =
    time.minute + toFloat time.hour


fromDecimal : Float -> Time
fromDecimal dec =
    let
        hour =
            floor dec
    in
    { hour = hour, minute = dec - toFloat hour }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ table []
            [ thead []
                [ tr []
                    [ th [] [ text "Time start" ]
                    , th [] [ text "Comment" ]
                    , th [] [ text "Duration" ]
                    , th [] [ text "Working time" ]
                    , th [] [ text "Actions" ]
                    ]
                ]
            , tbody [] (renderRows model)
            , tfoot []
                [ tr [ colspan 5 ]
                    [ td [] [ text "Sum" ]
                    ]
                ]
            ]
        , div []
            [ button [] [ text "Reset" ]
            , button [] [ text "Summary" ]
            ]
        ]


onBlurWithTargetValue : (String -> msg) -> Attribute msg
onBlurWithTargetValue tagger =
    on "blur" (Json.map tagger targetValue)


renderRows : Model -> List (Html Msg)
renderRows model =
    List.concat
        [ List.indexedMap (renderRow model.entries) model.entries
        , [ tr []
                [ td [] [ input [ placeholder "h:mm", onBlurWithTargetValue (ValidateTime Nothing) ] [] ]
                , td [] [ input [ placeholder "description" ] [] ]
                , td [] []
                , td [] []
                , td [] []
                ]
          ]
        ]


renderRow : List Entry -> Int -> Entry -> Html Msg
renderRow entries rowIndex entry =
    tr []
        [ td [] [ input [ value (renderTime entry.from), onBlurWithTargetValue (ValidateTime (Just rowIndex)) ] [] ]
        , td [] [ input [ placeholderOrValue entry.comment "comment" ] [] ]
        , td [] [ text (renderMaybeTime (duration entry (get rowIndex entries))) ]
        , td [] []
        , td [] []
        ]


get : Int -> List Entry -> Maybe Entry
get i entries =
    List.head (List.drop (i - 1) entries)


duration : Entry -> Maybe Entry -> Maybe Time
duration from maybeTo =
    case maybeTo of
        Nothing ->
            Nothing

        Just to ->
            Just (fromDecimal (toDecimal from.from - toDecimal to.from))


renderMaybeTime : Maybe Time -> String
renderMaybeTime mTime =
    case mTime of
        Nothing ->
            "-"

        Just time ->
            renderTime time


renderTime : Time -> String
renderTime time =
    String.fromInt time.hour ++ ":" ++ String.right 2 ("0" ++ String.fromFloat (time.minute * 60))


placeholderOrValue : String -> String -> Attribute Msg
placeholderOrValue string placeholderText =
    case string of
        "" ->
            placeholder placeholderText

        _ ->
            value string
