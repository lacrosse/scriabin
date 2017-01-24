module Models.Assemblage exposing (..)

-- MODEL

type Kind
  = Person
  | Composition
  | Recording

type alias Assemblage =
  { id : Int
  , name : String
  , kind : Kind
  , fileIds : List Int
  }

dummyComposer : Assemblage
dummyComposer =
  Assemblage 1 "Alexander Scriabin" Person []

dummyComposition : Assemblage
dummyComposition =
  Assemblage 2 "Piano Sonata No. 2" Composition []

dummyRecording : Assemblage
dummyRecording =
  Assemblage 3 "Maria Lettberg's recording" Recording [1]

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
