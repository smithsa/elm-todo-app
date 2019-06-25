module Main exposing (main)

import Browser
import Dom exposing (..)
import Html exposing (Html)
import Html.Attributes exposing (value, placeholder)
import List
import Maybe.Extra


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
        [ { id = 2, name = "Walk the Dog", description = "Dog needs to be waled everyday", status = Incomplete }
        , { id = 1, name = "Groceries", description = "Must pick up groceries", status = Incomplete }
        ]
    , inputTaskName = ""
    }


type Msg
    = AddTask
    | DeleteTask Task
    | CompleteTask Task
    | UndoTaskCompletion Task
    | UpdateInputTaskName String


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddTask ->
            { model
                | inputTaskName = ""
                , tasks = { id = nextTaskId model.tasks, name = model.inputTaskName, description = "", status = Incomplete } :: model.tasks
            }

        DeleteTask task ->
            { model
                | tasks = deleteTask task model.tasks
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


deleteTask : Task -> List Task -> List Task
deleteTask task tasks =
    List.filter
        (\currentTask -> currentTask.id /= task.id)
        tasks


nextTaskId : List Task -> Int
nextTaskId tasks =
    Maybe.Extra.unwrap 0 (\lastTaskAdded -> lastTaskAdded.id + 1) (List.head tasks)


view : Model -> Html Msg
view model =
    element "div"
        |> appendChildList
            [ element "div"
                |> appendChildList
                    [ element "input"
                        |> addAttributeList [
                            Html.Attributes.value model.inputTaskName,
                            Html.Attributes.placeholder "What needs to be done?"
                        ]
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


taskToggleCompleteButton : Task -> Element Msg
taskToggleCompleteButton task =
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
            , taskToggleCompleteButton task
            , element "button"
                |> appendText "Delete"
                |> addAction ("click", DeleteTask task)
            ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
