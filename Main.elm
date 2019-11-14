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
    {}


init : Model
init =
    {}



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


view : Model -> Html Msg
view model =
    Html.text "xx"



---- PROGRAM ----
-- main : Program () Model Msg


main =
    Browser.sandbox
        { view = view
        , init = init
        , update = update
        }
