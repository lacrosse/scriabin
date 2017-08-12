module Data.Account exposing (..)

import Json.Decode as JD

-- DECODERS

jsonDecoder : JD.Decoder String
jsonDecoder =
  JD.field "username" JD.string
