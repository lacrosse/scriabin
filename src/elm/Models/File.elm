module Models.File exposing (..)

import Json.Decode as JD

-- MODEL

type alias File =
  { id : Int
  , sha256 : String
  , mime : String
  , name : String
  }

dummy : File
dummy =
  File 1 "4e1243bd22c66e76c2ba9eddc1f91394e57f9f83" "audio/mpeg" "01.mp3"

-- DECODERS

jsonDecoder : JD.Decoder File
jsonDecoder =
  JD.map4 File
    (JD.field "id" JD.int)
    (JD.field "sha256" JD.string)
    (JD.field "mime" JD.string)
    (JD.field "name" JD.string)
