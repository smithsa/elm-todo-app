port module Types exposing (CompletionStatus(..), EditableString(..), Flags, Task, VisibleTasks(..), completionStatusDecoder, completionStatusEncoder, completionStatusFromString, editableStringDecoder, editableStringEncoder, taskDecoder, taskEncoder, tasksDecoders, tasksEncoder)

import Json.Decode
import Json.Encode


type alias Task =
    { id : Int, name : String, description : EditableString, status : CompletionStatus }


tasksEncoder : List Task -> Json.Encode.Value
tasksEncoder tasks =
    Json.Encode.list taskEncoder tasks


taskEncoder : Task -> Json.Encode.Value
taskEncoder task =
    Json.Encode.object
        [ ( "id", Json.Encode.int task.id )
        , ( "name", Json.Encode.string task.name )
        , ( "description", editableStringEncoder task.description )
        , ( "status", completionStatusEncoder task.status )
        ]


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


type CompletionStatus
    = Complete
    | Incomplete


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


completionStatusEncoder : CompletionStatus -> Json.Encode.Value
completionStatusEncoder completionStatus =
    case completionStatus of
        Complete ->
            Json.Encode.string "complete"

        Incomplete ->
            Json.Encode.string "incomplete"


type EditableString
    = NotEditing String
    | Editing String String


editableStringDecoder : Json.Decode.Decoder EditableString
editableStringDecoder =
    Json.Decode.string
        |> Json.Decode.map NotEditing


editableStringEncoder : EditableString -> Json.Encode.Value
editableStringEncoder editableString =
    case editableString of
        Editing val _ ->
            Json.Encode.string val

        NotEditing val ->
            Json.Encode.string val


type VisibleTasks
    = AllTasks
    | CompleteTasks
    | IncompleteTasks


type alias Flags =
    { tasks : Json.Decode.Value
    }
