module Models.Assemblage exposing (..)

import Json.Decode as JD

-- MODEL

type Kind
  = Person
  | Composition
  | Recording
  | General

type alias Assemblage =
  { id : Int
  , name : String
  , kind : Kind
  , fileIds : List Int
  , tagIds : List Int
  }

-- FUNCTIONS

isComposer : Assemblage -> Bool
isComposer { kind } =
  case kind of
    Person ->
      True
    _ ->
      False

fullName : Assemblage -> String
fullName { name } =
  name

parseKind : String -> Kind
parseKind string =
  case string of
    "person" -> Person
    "composition" -> Composition
    "recording" -> Recording
    _ -> General

-- DECODERS

jsonDecoder : JD.Decoder Assemblage
jsonDecoder =
  JD.map5 Assemblage
    (JD.field "id" JD.int)
    (JD.field "name" JD.string)
    ((JD.map parseKind << JD.field "kind") JD.string)
    ((JD.map (Maybe.withDefault []) << JD.maybe << JD.field "file_ids" << JD.list) JD.int)
    ((JD.map (Maybe.withDefault []) << JD.maybe << JD.field "tag_ids" << JD.list) JD.int)
