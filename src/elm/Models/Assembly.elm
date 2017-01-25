module Models.Assembly exposing (..)

import Json.Decode as JD

-- MODEL

type Kind
  = Composed
  | Recorded
  | Reconstructed
  | General

type alias Assembly =
  { assemblageId : Int
  , kind : Kind
  , childAssemblageId : Int
  }

dummyComposed : Assembly
dummyComposed =
  Assembly 1 Composed 2

dummyRecorded : Assembly
dummyRecorded =
  Assembly 2 Recorded 3

-- FUNCTIONS

parseKind : String -> Kind
parseKind string =
  case string of
    "composed" -> Composed
    "recorded" -> Recorded
    "reconstructed" -> Reconstructed
    _ -> General

-- DECODERS

jsonDecoder : JD.Decoder Assembly
jsonDecoder =
  JD.map3 Assembly
    (JD.field "assemblage_id" JD.int)
    (JD.map parseKind (JD.field "kind" JD.string))
    (JD.field "child_assemblage_id" JD.int)
