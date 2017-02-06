module Models.Assembly exposing (..)

import Json.Decode as JD

-- MODEL

type Kind
  = Composed
  | Recorded
  | Reconstructed
  | Performed
  | General

type alias Assembly =
  { assemblageId : Int
  , kind : Kind
  , childAssemblageId : Int
  }

-- FUNCTIONS

parseKind : String -> Kind
parseKind string =
  case string of
    "composed" -> Composed
    "recorded" -> Recorded
    "performed" -> Performed
    "reconstructed" -> Reconstructed
    _ -> General

-- DECODERS

jsonDecoder : JD.Decoder Assembly
jsonDecoder =
  JD.map3 Assembly
    (JD.field "assemblage_id" JD.int)
    (JD.map parseKind (JD.field "kind" JD.string))
    (JD.field "child_assemblage_id" JD.int)
