port module Types exposing (..)

import Json.Decode

type alias Task =
    { id : Int, name : String, description : EditableString, status : CompletionStatus }


type CompletionStatus
    = Complete
    | Incomplete


type EditableString
    = NotEditing String
    | Editing String String


type VisibleTasks
    = AllTasks
    | CompleteTasks
    | IncompleteTasks

type alias Flags =
    {
        tasks :  Json.Decode.Value
    }