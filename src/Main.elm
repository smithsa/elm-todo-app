module Main exposing (main)

import Browser
import Dom exposing (..)
import Html exposing (Html)
import Html.Attributes exposing (placeholder, value)
import List
import Maybe.Extra
import Json.Decode
import Json.Encode
import Ports
import Types exposing (..)

type alias Model =
    { tasks : List Task
    , inputTaskName : String
    , visibleTasks : VisibleTasks
    }


tasksDecoders : Json.Decode.Decoder (List Task)
tasksDecoders =
    Json.Decode.list taskDecoder

taskDecoder : Json.Decode.Decoder Task
taskDecoder =
    Json.Decode.map4 Task
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "description" editableStringDecoder)
        (Json.Decode.field "status" completionStatusDecoder)

editableStringDecoder : Json.Decode.Decoder EditableString
editableStringDecoder =
    Json.Decode.string
        |> Json.Decode.map NotEditing

completionStatusDecoder : Json.Decode.Decoder CompletionStatus
completionStatusDecoder =
    Json.Decode.string
        |> Json.Decode.map completionStatusFromString

completionStatusFromString : String -> CompletionStatus
completionStatusFromString completionStatusString =
    case completionStatusString of
        "complete" ->
            Complete

        "incomplete" ->
            Incomplete
        _ ->
            Incomplete

tasksEncoder : List Task -> Json.Encode.Value
tasksEncoder tasks =
     Json.Encode.list taskEncoder tasks


taskEncoder: Task -> Json.Encode.Value
taskEncoder task =
    Json.Encode.object
        [ ( "id",  Json.Encode.int task.id)
        , ( "name", Json.Encode.string task.name)
        , ( "description", editableStringEncoder task.description)
        , ( "status", completionStatusEncoder task.status)
        ]

editableStringEncoder : EditableString -> Json.Encode.Value
editableStringEncoder editableString =
    case editableString of
        Editing val bufferVal ->
            Json.Encode.string val

        NotEditing val ->
            Json.Encode.string val


completionStatusEncoder : CompletionStatus -> Json.Encode.Value
completionStatusEncoder completionStatus =
    case completionStatus of
        Complete ->
            Json.Encode.string "complete"

        Incomplete ->
            Json.Encode.string "incomplete"

initialModel : Flags -> (Model, Cmd Msg)
initialModel flags =
    let
        _ = Debug.log "flags" flags.tasks
    in
        case Json.Decode.decodeValue tasksDecoders flags.tasks of
            Ok tasks ->
                ({ tasks = tasks
                , inputTaskName = ""
                , visibleTasks = AllTasks
                }
                , Cmd.none)
            Err err ->
                let
                    _ = Debug.log "error" err
                in
                    ({ tasks = []
                    , inputTaskName = ""
                    , visibleTasks = AllTasks
                    }
                    , Cmd.none)

type Msg
    = AddTask
    | DeleteTask Int
    | CompleteTask Int
    | UndoTaskCompletion Int
    | UpdateInputTaskName String
    | TriggerDescriptionEdit Int
    | EditDescription Int
    | UpdateTaskDescriptionBuffer Int String
    | CancelEditDescription Int
    | UpdateVisibleTasks VisibleTasks


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, commands ) =
            update msg model

        extractedTasks =
            tasksEncoder newModel.tasks

    in
        ( newModel
        , Cmd.batch [ commands, Ports.storeTasks extractedTasks ]
        )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddTask ->
            ({ model
                | inputTaskName = ""
                , tasks = { id = nextTaskId model.tasks, name = model.inputTaskName, description = NotEditing "", status = Incomplete } :: model.tasks
            }
            , Cmd.none)

        DeleteTask taskId ->
            ({ model
                | tasks = deleteTask taskId model.tasks
            }
            , Cmd.none)
        CompleteTask taskId ->
            ({ model
                | tasks = List.map (updateTaskStatus taskId Complete) model.tasks
            }
            , Cmd.none)
        UndoTaskCompletion taskId ->
            ({ model
                | tasks = List.map (updateTaskStatus taskId Incomplete) model.tasks
            }
            , Cmd.none)
        UpdateInputTaskName name ->
            ({ model
                | inputTaskName = name
            }
            , Cmd.none)
        TriggerDescriptionEdit taskId ->
            ({ model
                | tasks = List.map (triggerEditableDescription taskId) model.tasks
            }
            , Cmd.none)
        EditDescription taskId ->
            ({ model
                | tasks = List.map (editDescription taskId) model.tasks
            }
            , Cmd.none)
        UpdateTaskDescriptionBuffer taskId val ->
           ( { model
                | tasks = List.map (updateTaskDescriptionBuffer taskId val) model.tasks
            }
            , Cmd.none)
        CancelEditDescription taskId ->
            ({ model
                | tasks = List.map (cancelEditDescription taskId) model.tasks
            }
            , Cmd.none)
        UpdateVisibleTasks visibleTasksType ->
            ({ model
                | visibleTasks = visibleTasksType
            }
            , Cmd.none)

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
            Editing val _ ->
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


cancelEditDescription : Int -> Task -> Task
cancelEditDescription taskId task =
    if taskId == task.id then
        case task.description of
            Editing val _ ->
                { task | description = NotEditing val }

            NotEditing _ ->
                task

    else
        task


deleteTask : Int -> List Task -> List Task
deleteTask taskId tasks =
    List.filter
        (\currentTask -> currentTask.id /= taskId)
        tasks


nextTaskId : List Task -> Int
nextTaskId tasks =
    Maybe.Extra.unwrap 0 (\lastTaskAdded -> lastTaskAdded.id + 1) (List.head tasks)


view : Model -> Html Msg
view model =
    element "div"
        |> addClass "todo"
        |> appendChildList
            [ taskFilterElements
            , element "div"
                |> appendChildList
                    [ element "input"
                        |> addClass "addtask"
                        |> addAttributeList
                            [ Html.Attributes.value model.inputTaskName
                            , Html.Attributes.placeholder "What needs to be done?"
                            ]
                        |> addInputHandler UpdateInputTaskName
                    , element "button"
                        |> addClass "addtask"
                        |> appendText "Add Task"
                        |> addAction ( "click", AddTask )
                    ]
            , element "ul"
                |> appendChildList
                    (filteredTaskListElements model.visibleTasks model.tasks
                        |> List.map taskListElement
                    )
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
            |> addClass "undo"
            |> addAction ( "click", UndoTaskCompletion task.id )

    else
        element "button"
            |> appendText "Complete"
            |> addClass "complete"
            |> addAction ( "click", CompleteTask task.id )


filteredTaskListElements : VisibleTasks -> List Task -> List Task
filteredTaskListElements visibleTask tasks =
    case visibleTask of
        AllTasks ->
            tasks

        CompleteTasks ->
            List.filter (\task -> task.status == Complete) tasks

        IncompleteTasks ->
            List.filter (\task -> task.status == Incomplete) tasks


taskListElement : Task -> Element Msg
taskListElement task =
    element "li"
        |> appendChildList
            [ taskName task
            , taskToggleCompleteButton task
            , element "span"
                |> addClass "delete"
                |> addAction ( "click", DeleteTask task.id )
            , taskDescriptionElement task
            ]


taskDescriptionElement : Task -> Element Msg
taskDescriptionElement task =
    if not <| taskComplete task then
        case task.description of
            NotEditing val ->
                element "div"
                    |> addClass "description-container"
                    |> appendText val
                    |> addAction ( "click", TriggerDescriptionEdit task.id )

            Editing _ bufferVal ->
                element "div"
                    |> addClass "description-container"
                    |> appendChildList
                        [ element "input"
                            |> addInputHandler (UpdateTaskDescriptionBuffer task.id)
                            |> addClass "description"
                            |> addAttributeList
                                [ Html.Attributes.value bufferVal
                                , Html.Attributes.contenteditable True
                                , Html.Attributes.autofocus True
                                ]
                        , element "button"
                            |> appendText "Save"
                            |> addClass "save"
                            |> addAction ( "click", EditDescription task.id )
                        , element "button"
                            |> appendText "Cancel"
                            |> addClass "cancel"
                            |> addAction ( "click", CancelEditDescription task.id )
                        ]

    else
        element "div"


taskFilterElements : Element Msg
taskFilterElements =
    element "div"
        |> addClass "filter"
        |> appendChildList
            [ element "button"
                |> appendText "All Tasks"
                |> addAction ( "click", UpdateVisibleTasks AllTasks )
            , element "button"
                |> appendText "Completed Tasks"
                |> addAction ( "click", UpdateVisibleTasks CompleteTasks )
            , element "button"
                |> appendText "Incomplete Tasks"
                |> addAction ( "click", UpdateVisibleTasks IncompleteTasks )
            ]



main : Program Flags Model Msg
main =
    Browser.document
        { init = initialModel
        , view = \model -> { title = "Elm Todo App", body = [view model] }
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        }
