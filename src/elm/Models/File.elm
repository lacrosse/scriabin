module Models.File exposing (..)

import Json.Decode as JD

-- MODEL

type alias File =
  { id : Int
  , sha256 : String
  , mime : String
  , name : String
  , title : String
  }

-- DECODERS

jsonDecoder : JD.Decoder File
jsonDecoder =
  JD.map5 File
    (JD.field "id" JD.int)
    (JD.field "sha256" JD.string)
    (JD.field "mime" JD.string)
    (JD.field "name" JD.string)
    (JD.field "title" JD.string)
