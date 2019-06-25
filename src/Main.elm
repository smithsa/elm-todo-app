module Main exposing (main)

import Browser
import Dom exposing (..)
import Html exposing (Html)
import List
import Maybe.Extra as Maybe


type alias Task =
    { id : Int, name : String, description : String, status : CompletionStatus }


type CompletionStatus
    = Complete
    | Incomplete


type alias Model =
    { tasks : List Task
    , inputTaskName : String
    }


initialModel : Model
initialModel =
    { tasks =
        [ { id = 1, name = "Walk the Dog", description = "Dog needs to be waled everyday", status = Incomplete }
        , { id = 2, name = "Groceries", description = "Must pick up groceries", status = Incomplete }
        ]
    , inputTaskName = ""
    }


type Msg
    = AddTask
    | CompleteTask Task
    | UndoTaskCompletion Task
    | UpdateInputTaskName String


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddTask ->
            { model
                | tasks = { id = nextTaskId model.tasks, name = model.inputTaskName, description = "", status = Incomplete } :: model.tasks
            }

        CompleteTask task ->
            { model
                | tasks = List.map (updateTaskStatus task.id Complete) model.tasks
            }

        UndoTaskCompletion task ->
            { model
                | tasks = List.map (updateTaskStatus task.id Incomplete) model.tasks
            }

        UpdateInputTaskName name ->
            { model
                | inputTaskName = name
            }


updateTaskStatus : Int -> CompletionStatus -> Task -> Task
updateTaskStatus taskId completionStatus task =
    if taskId == task.id then
        { task | status = completionStatus }

    else
        task


nextTaskId : List Task -> Int
nextTaskId tasks =
    let
        reveredTasks =
            List.reverse tasks
    in
    case List.head reveredTasks of
        Just lastTaskAdded ->
            lastTaskAdded.id + 1

        Nothing ->
            0


view : Model -> Html Msg
view model =
    element "div"
        |> appendChildList
            [ element "div"
                |> appendChildList
                    [ element "input"
                        |> addInputHandler UpdateInputTaskName
                    , element "button"
                        |> appendText "Add Task"
                        |> addAction ( "click", AddTask )
                    ]
            , element "ul"
                |> appendChildList
                    (List.map taskListElement model.tasks)
            ]
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
        |> addStyleConditional ( "text-decoration", "line-through" ) (taskComplete task)


taskToggleButton : Task -> Element Msg
taskToggleButton task =
    if taskComplete task then
        element "button"
            |> appendText "Undo"
            |> addAction ( "click", UndoTaskCompletion task )

    else
        element "button"
            |> appendText "Complete"
            |> addAction ( "click", CompleteTask task )


taskListElement : Task -> Element Msg
taskListElement task =
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
