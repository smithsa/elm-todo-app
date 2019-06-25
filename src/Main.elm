module Main exposing (main)

import Browser
import Dom exposing (..)
import Html exposing (Html)
import List


type alias Task =
    { name : String, description : String, status : CompletionStatus }


type CompletionStatus
    = Complete
    | Incomplete


type alias Model =
    { tasks : List Task }


initialModel : Model
initialModel =
    { tasks =
        [ { name = "Walk the Dog", description = "Dog needs to be waled everyday", status = Incomplete }
        , { name = "Groceries", description = "Must pick up groceries", status = Incomplete }
        ]
    }


type Msg
    = AddTask
    | CompleteTask


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddTask ->
            model
        CompleteTask ->
            model


view : Model -> Html Msg
view model =
    element "ul"
        |> appendChildList
            (List.map taskElement model.tasks)
        |> render

taskComplete : Task -> Bool
taskComplete task =
    if task.status == Complete then
        True
    else
        False

taskName : Task -> Element Msg
taskName task =
            element "span"
            |> appendText task.name
            |> addStyleConditional ("text-decoration", "line-through") (taskComplete task)


taskToggleButton : Task -> Element Msg
taskToggleButton task =
       if taskComplete task then
        element "button"
            |> appendText "Undo"
            |> addAction ("click", CompleteTask)

       else
        element "button"
            |> appendText "Complete"
            |> addAction ("click", CompleteTask)

taskElement : Task -> Element Msg
taskElement task =
    element "li"
        |> appendChildList
            [ taskName task
            , taskToggleButton task
            ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
