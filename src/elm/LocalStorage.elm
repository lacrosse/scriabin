port module LocalStorage exposing (..)

import Json.Encode as JE

port localStorage : JE.Value -> Cmd msg

-- FUNCTIONS

set : String -> String -> Cmd msg
set key value =
  localStorage
    <| JE.object
      [ ("action", JE.string "set")
      , ("key", JE.string key)
      , ("value", JE.string value)
      ]

remove : String -> Cmd msg
remove key =
  localStorage
    <| JE.object
      [ ("action", JE.string "remove")
      , ("key", JE.string key)
      ]
