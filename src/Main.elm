module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Dom

type alias Task = { name: String }

type alias Model =
    { tasks : List Task }


initialModel : Model
initialModel =
    { tasks = [ {name = "Walk the Dog"} ] }


type Msg
    = AddTask


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddTask ->
            model

view : Model -> Html Msg
view model =
    div [] []


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
