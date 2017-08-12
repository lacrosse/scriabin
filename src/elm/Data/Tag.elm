module Data.Tag exposing (..)

import Json.Decode as JD

-- MODEL

type alias Tag =
  { id : Int
  , key : String
  , value : String
  }

-- DECODERS

jsonDecoder : JD.Decoder Tag
jsonDecoder =
  JD.map3 Tag
    (JD.field "id" JD.int)
    (JD.field "key" JD.string)
    (JD.field "value" JD.string)
