port module Ports exposing (..)

import Json.Encode

port storeTasks : Json.Encode.Value -> Cmd msg