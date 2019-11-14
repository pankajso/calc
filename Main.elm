module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html)
import Html.Attributes as HA
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


update : Msg -> Model -> Model
update message model =
    let
        noChange =
            model
    in
    case message of
        NoOp ->
            model



---- VIEW ----
-- button [onClick msg, style btnStyle] [text string]


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div []
            [ Html.div [] [ Html.text "0" ]
            , Html.div []
                [ Html.button [] [ Html.text "C" ]
                , Html.button [] [ Html.text "-" ]
                , Html.button [] [ Html.text "%" ]
                , Html.button [] [ Html.text "/" ]
                ]
            , Html.div []
                [ Html.button [] [ Html.text "7" ]
                , Html.button [] [ Html.text "8" ]
                , Html.button [] [ Html.text "9" ]
                , Html.button [] [ Html.text "X" ]
                ]
            , Html.div []
                [ Html.button [] [ Html.text "4" ]
                , Html.button [] [ Html.text "5" ]
                , Html.button [] [ Html.text "6" ]
                , Html.button [] [ Html.text "-" ]
                ]
            , Html.div []
                [ Html.button [] [ Html.text "1" ]
                , Html.button [] [ Html.text "2" ]
                , Html.button [] [ Html.text "3" ]
                , Html.button [] [ Html.text "+" ]
                ]
            , Html.div []
                [ Html.button [] [ Html.text "0" ]
                , Html.button [] [ Html.text "." ]
                , Html.button [] [ Html.text "=" ]
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
