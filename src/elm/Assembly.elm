module Assembly exposing (..)

-- MODEL

type Kind
  = Composed
  | Recorded
  | Reconstructed

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
