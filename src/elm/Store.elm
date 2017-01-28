module Store exposing (..)

import Models.Assemblage as Assemblage exposing (Assemblage)
import Models.Assembly as Assembly exposing (Assembly)
import Models.File as File exposing (File)
import Dict exposing (Dict)
import Jwt
import Celeste

-- MODEL

type alias Store =
  { assemblages : Dict Int Assemblage
  , assemblies : Dict (Int, Int) Assembly
  , files : Dict Int File
  }

type alias Model =
  Store

type Relation
  = Assemblage
  | Assembly
  | File

type alias CompositeKey = (Int, Int)

type PrimaryKey
  = Int
  | CompositeKey

initialModel : Model
initialModel =
  { assemblages = Dict.fromList []
  , assemblies = Dict.fromList []
  , files = Dict.fromList []
  }

-- FUNCTIONS

fetch : (Result Jwt.JwtError Celeste.Response -> msg) -> String -> Celeste.Route -> String -> Cmd msg
fetch handler apiBase route jwt =
  Jwt.send handler (Jwt.get jwt (Celeste.route apiBase route) (Celeste.decoder route))
