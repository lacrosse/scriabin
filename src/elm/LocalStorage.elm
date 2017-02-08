port module LocalStorage exposing (..)

import Json.Encode as JE

port localStorage : JE.Value -> Cmd msg
