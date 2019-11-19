module Main exposing (..)

import Browser
import Browser.Events exposing (onClick, onKeyDown)
import Browser.Navigation as Nav
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events exposing (keyCode, on, onClick, onInput)
import Json.Decode as Json
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Url
import Url.Parser as Parser exposing ((</>), (<?>), Parser, custom, fragment, map, oneOf, s, top)
import Url.Parser.Query as Query



---- MODEL ----


type alias Model =
    { io : String
    , prev : Float
    , operation : Operation
    }


init : ( Model, Cmd Msg )
init =
    ( { io = "0"
      , prev = 0
      , operation = None
      }
    , Cmd.none
    )



-- type Key
--     = Character Char
--     | Control String
-- keyDecoder : Json.Decoder Key
-- keyDecoder =
--     Json.map toKey (Json.field "key" Json.string)
--
--
-- toKey : String -> Key
-- toKey string =
--     case String.uncons string of
--         Just ( char, "" ) ->
--             Character char
--
--         _ ->
--             Control string
-- % + - *


type Operation
    = Div
    | Mult
    | Add
    | Subst
    | None



---- UPDATE ----


type Msg
    = NoOp
    | Number Float
    | Op Operation
    | Result
    | Clear
    | Dot
    | HandleKeyboardEvent KeyboardEvent



-- | UpdateCurrentItem String
-- | AddItem


add : number -> number -> number
add x y =
    calc (-) x y


calc : (a -> b -> c) -> a -> b -> c
calc f x y =
    f x y


operate : Operation -> Float -> String -> String
operate operation x y_ =
    let
        y =
            stringToFloat y_
    in
    (case operation of
        Mult ->
            (*) x y

        Div ->
            (/) x y

        Add ->
            (/) x y

        Subst ->
            (-) x y

        None ->
            x
    )
        |> String.fromFloat


stringToFloat : String -> Float
stringToFloat x =
    -- Maybe.withDefault 0 (String.toFloat x)
    -- String.toFloat x
    --     |> Maybe.withDefault 0
    --     |> (*) 10
    --     |> (+) 1
    --     |> (/) 20
    --     |> negate
    String.toFloat x
        |> (\i -> Maybe.withDefault 0 i)
        |> (*) 10


wd i =
    Maybe.withDefault 0 i


floatToString : Float -> String
floatToString d =
    String.fromFloat d


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    let
        noChange =
            ( model, Cmd.none )
    in
    case message of
        NoOp ->
            ( model, Cmd.none )

        Number num ->
            let
                -- newModel =
                --     { model | io = String.fromFloat (Maybe.withDefault 0 (String.toFloat model.io) + num) }
                newModel =
                    { model
                        | io =
                            -- if model.prev == "0" || model.io /= "0" then
                            if model.io == "0" then
                                String.fromFloat num

                            else
                                model.io ++ String.fromFloat num

                        -- else
                        -- String.fromFloat num
                    }
            in
            ( newModel, Cmd.none )

        Op operation ->
            let
                newModel =
                    { model
                        | operation = operation
                        , prev = Maybe.withDefault 0 (String.toFloat model.io)
                        , io = "0"
                    }
            in
            ( newModel, Cmd.none )

        Result ->
            let
                newModel =
                    { model
                        | io =
                            operate model.operation model.prev model.io
                    }
            in
            ( newModel, Cmd.none )

        Clear ->
            let
                newModel =
                    { model | io = "0", operation = None, prev = 0 }
            in
            ( newModel, Cmd.none )

        Dot ->
            let
                newModel =
                    { model
                        | io =
                            if model.io == "0" then
                                "0."

                            else if String.contains "." model.io then
                                model.io

                            else
                                model.io ++ "."
                    }
            in
            ( newModel, Cmd.none )

        -- HandleKeyboardEvent event ->
        --     ( { model | lastEvent = Just event }
        --     , Cmd.none
        --     )
        HandleKeyboardEvent event ->
            let
                newModel =
                    { model
                        | io =
                            -- if
                            --     (Maybe.withDefault 0
                            --         (String.toFloat (Maybe.withDefault "" event.key))
                            --         <= 9
                            --     )
                            --         && (Maybe.withDefault 0 (String.toFloat (Maybe.withDefault "" event.key)) >= 0)
                            -- then
                            --     model.io ++ Maybe.withDefault "" event.key
                            --
                            -- else
                            --     model.io
                            case event.key of
                                Just a ->
                                    if (stringToFloat a <= 9) && (stringToFloat a >= 0) then
                                        model.io ++ a

                                    else
                                        model.io

                                Nothing ->
                                    model.io

                        -- case event.key of
                        --     Just "0" ->
                        --         model.io ++ "0"
                        --
                        --     Just "1" ->
                        --         model.io ++ "1"
                        --
                        --     Nothing ->
                        --         model.io
                        --
                        --     Just _ ->
                        --         model.io
                    }
            in
            Debug.log (Debug.toString event)
                ( newModel
                , Cmd.none
                )



-- UpdateCurrentItem _ ->
--     Debug.todo "handle UpdateCurrentItem _"
--
-- AddItem ->
--     Debug.todo "handle AddItem"
-- onEnter : msg -> Html.Attribute msg
-- onEnter msg =
--     let
--         isEnterKey keyCode =
--             if keyCode >= 48 && keyCode <= 57 then
--                 Json.succeed msg
--
--             else
--                 Json.fail "silent failure :)"
--     in
--     on "keyup" <|
--         Json.andThen isEnterKey Html.Events.keyCode


subscriptions : Model -> Sub Msg
subscriptions model =
    onKeyDown (Json.map HandleKeyboardEvent decodeKeyboardEvent)



---- VIEW ----


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div []
            [ Html.div [] [ Html.text model.io ]
            , Html.div []
                [ Html.button [ onClick Clear ] [ Html.text "C" ]
                , Html.button [] [ Html.text "-" ]
                , Html.button [] [ Html.text "%" ]
                , Html.button [ onClick (Op Div) ] [ Html.text "/" ]
                ]
            , Html.div []
                [ numBtn 7
                , numBtn 8
                , numBtn 9
                , Html.button [ onClick (Op Mult) ] [ Html.text "X" ]
                ]
            , Html.div []
                [ numBtn 4
                , numBtn 5
                , numBtn 6
                , Html.button [ onClick (Op Subst) ] [ Html.text "-" ]
                ]
            , Html.div []
                [ numBtn 1
                , numBtn 2
                , numBtn 3
                , Html.button [ onClick (Op Add) ] [ Html.text "+" ]
                ]
            , Html.div []
                [ numBtn 0
                , Html.button [ onClick Dot ] [ Html.text "." ]
                , Html.button [ onClick Result ] [ Html.text "=" ]
                ]
            ]
        ]


numBtn : Float -> Html Msg
numBtn n =
    Html.button [ numClick n ] [ n |> String.fromFloat |> Html.text ]


numClick : Float -> Html.Attribute Msg
numClick n =
    onClick (Number n)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
