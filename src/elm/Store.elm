module Store exposing (..)

import Assemblage exposing (Assemblage)
import Assembly exposing (Assembly)
import File exposing (File)

-- MODEL

type alias Store =
  { assemblages : List Assemblage
  , assemblies : List Assembly
  , files : List File
  }

type alias Model =
  Store

initialModel : Model
initialModel =
  { assemblages =
    [ Assemblage.dummyComposer
    , Assemblage.dummyComposition
    , Assemblage.dummyRecording
    ]
  , assemblies =
    [ Assembly.dummyComposed
    , Assembly.dummyRecorded
    ]
  , files = [File.dummy]
  }
