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
                            case model.operation of
                                Mult ->
                                    String.fromFloat (Maybe.withDefault 0 (String.toFloat model.io) * model.prev)

                                Add ->
                                    String.fromFloat (Maybe.withDefault 0 (String.toFloat model.io) + model.prev)

                                Div ->
                                    String.fromFloat (model.prev / Maybe.withDefault 0 (String.toFloat model.io))

                                Subst ->
                                    String.fromFloat (model.prev - Maybe.withDefault 0 (String.toFloat model.io))

                                None ->
                                    model.io
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
                            if (Maybe.withDefault 0 (String.toFloat (Maybe.withDefault "" event.key)) <= 9) && (Maybe.withDefault 0 (String.toFloat (Maybe.withDefault "" event.key)) >= 0) then
                                model.io ++ Maybe.withDefault "" event.key

                            else
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
-- button [onClick msg, style btnStyle] [text string]


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
                [ Html.button [ onClick (Number 7) ] [ Html.text "7" ]
                , Html.button [ onClick (Number 8) ] [ Html.text "8" ]
                , Html.button [ onClick (Number 9) ] [ Html.text "9" ]
                , Html.button [ onClick (Op Mult) ] [ Html.text "X" ]
                ]
            , Html.div []
                [ Html.button [ onClick (Number 4) ] [ Html.text "4" ]
                , Html.button [ onClick (Number 5) ] [ Html.text "5" ]
                , Html.button [ onClick (Number 6) ] [ Html.text "6" ]
                , Html.button [ onClick (Op Subst) ] [ Html.text "-" ]
                ]
            , Html.div []
                [ Html.button [ onClick (Number 1) ] [ Html.text "1" ]
                , Html.button [ onClick (Number 2) ] [ Html.text "2" ]
                , Html.button [ onClick (Number 3) ] [ Html.text "3" ]
                , Html.button [ onClick (Op Add) ] [ Html.text "+" ]
                ]
            , Html.div []
                [ Html.button [ onClick (Number 0) ] [ Html.text "0" ]
                , Html.button [ onClick Dot ] [ Html.text "." ]
                , Html.button [ onClick Result ] [ Html.text "=" ]
                ]
            ]
        ]



---- PROGRAM ----
-- main : Program () Model Msg


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



--
-- main =
--     Browser.sandbox
--         { view = view
--         , init = init
--         , subscriptions = subscriptions
--         , update = update
--         }
