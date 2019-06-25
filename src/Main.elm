module Main exposing (main)

import Browser
import Dom exposing (..)
import Html exposing (Html)
import Html.Attributes exposing (placeholder, value)
import List
import Maybe.Extra


type alias Task =
    { id : Int, name : String, description : EditableString, status : CompletionStatus }


type CompletionStatus
    = Complete
    | Incomplete


type EditableString
    = NotEditing String
    | Editing String String


type alias Model =
    { tasks : List Task
    , inputTaskName : String
    , editableDescriptionValue : String
    }


initialModel : Model
initialModel =
    { tasks =
        [ { id = 2, name = "Walk the Dog", description = NotEditing "Dog needs to be waled everyday", status = Incomplete }
        , { id = 1, name = "Groceries", description = NotEditing "Must pick up groceries", status = Incomplete }
        ]
    , inputTaskName = ""
    , editableDescriptionValue = ""
    }


type Msg
    = AddTask
    | DeleteTask Task
    | CompleteTask Task
    | UndoTaskCompletion Task
    | UpdateInputTaskName String
    | TriggerDescriptionEdit Task
    | EditDescription Task
    | UpdateTaskDescriptionBuffer Task String


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddTask ->
            { model
                | inputTaskName = ""
                , tasks = { id = nextTaskId model.tasks, name = model.inputTaskName, description = NotEditing "", status = Incomplete } :: model.tasks
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

        TriggerDescriptionEdit task ->
            { model
                | tasks = List.map (triggerEditableDescription task.id) model.tasks
            }

        EditDescription task ->
            { model
                | tasks = List.map (editDescription task.id) model.tasks
            }

        UpdateTaskDescriptionBuffer task val ->
            { model
                | tasks = List.map (updateTaskDescriptionBuffer task.id val) model.tasks
            }


updateTaskStatus : Int -> CompletionStatus -> Task -> Task
updateTaskStatus taskId completionStatus task =
    if taskId == task.id then
        { task | status = completionStatus }

    else
        task


triggerEditableDescription : Int -> Task -> Task
triggerEditableDescription taskId task =
    if taskId == task.id then
        case task.description of
            Editing val bufferVal ->
                { task | description = NotEditing val }

            NotEditing val ->
                { task | description = Editing val val }

    else
        task


editDescription : Int -> Task -> Task
editDescription taskId task =
    if taskId == task.id then
        case task.description of
            Editing _ bufferVal ->
                { task | description = NotEditing bufferVal }

            NotEditing _ ->
                task

    else
        task


updateTaskDescriptionBuffer : Int -> String -> Task -> Task
updateTaskDescriptionBuffer taskId inputVal task =
    if taskId == task.id then
        case task.description of
            Editing val _ ->
                { task | description = Editing val inputVal }

            NotEditing _ ->
                task

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
                        |> addAttributeList
                            [ Html.Attributes.value model.inputTaskName
                            , Html.Attributes.placeholder "What needs to be done?"
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
                |> addAction ( "click", DeleteTask task )
            , taskDescriptionElement task
            ]


taskDescriptionElement : Task -> Element Msg
taskDescriptionElement task =
    case task.description of
        NotEditing val ->
            element "div"
                |> addClass "description"
                |> appendText val
                |> addAction ( "click", TriggerDescriptionEdit task )

        Editing val bufferVal ->
            element "div"
                |> appendChildList
                    [ element "input"
                        |> addInputHandler (UpdateTaskDescriptionBuffer task)
                        |> addClass "description"
                        |> addAttributeList
                            [ Html.Attributes.value bufferVal
                            , Html.Attributes.contenteditable True
                            ]
                    , element "button"
                        |> appendText "Save"
                        |> addAction ( "click", EditDescription task )
                    ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
