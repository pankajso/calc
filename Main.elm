module Main exposing (..)

import Browser
import Browser.Events exposing (onClick)
import Browser.Navigation as Nav
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events exposing (onClick)
import Url
import Url.Parser as Parser exposing ((</>), (<?>), Parser, custom, fragment, map, oneOf, s, top)
import Url.Parser.Query as Query



---- MODEL ----


type alias Model =
    { io : String
    , prev : String
    , operation : Operation
    }


init : Model
init =
    { io = "0"
    , prev = "0"
    , operation = None
    }



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


update : Msg -> Model -> Model
update message model =
    let
        noChange =
            model
    in
    case message of
        NoOp ->
            model

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
            newModel

        Op operation ->
            let
                newModel =
                    { model
                        | operation = operation
                        , prev = model.io
                        , io = "0"
                    }
            in
            newModel

        Result ->
            let
                newModel =
                    { model
                        | io =
                            case model.operation of
                                Mult ->
                                    String.fromFloat (Maybe.withDefault 0 (String.toFloat model.prev) * Maybe.withDefault 0 (String.toFloat model.io))

                                Add ->
                                    String.fromFloat (Maybe.withDefault 0 (String.toFloat model.prev) + Maybe.withDefault 0 (String.toFloat model.io))

                                Div ->
                                    String.fromFloat (Maybe.withDefault 0 (String.toFloat model.prev) / Maybe.withDefault 0 (String.toFloat model.io))

                                Subst ->
                                    String.fromFloat (Maybe.withDefault 0 (String.toFloat model.prev) - Maybe.withDefault 0 (String.toFloat model.io))

                                None ->
                                    model.io
                    }
            in
            newModel

        Clear ->
            let
                newModel =
                    { model | io = "0", operation = None, prev = "0" }
            in
            newModel



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
                , Html.button [] [ Html.text "." ]
                , Html.button [ onClick Result ] [ Html.text "=" ]
                ]
            ]
        ]



---- PROGRAM ----
-- main : Program () Model Msg


main =
    Browser.sandbox
        { view = view
        , init = init
        , update = update
        }
