module Store exposing (..)

import Models.Assemblage as Assemblage exposing (Assemblage)
import Models.Assembly as Assembly exposing (Assembly)
import Models.File as File exposing (File)
import Models.Tag as Tag exposing (Tag)
import Dict exposing (Dict)
import Jwt
import Celeste

-- MODEL

type alias Store =
  { assemblages : Dict Int Assemblage
  , assemblies : Dict (Int, Int) Assembly
  , files : Dict Int File
  , tags : Dict Int Tag
  }

type alias Model =
  Store

type alias CompositeKey = (Int, Int)

type PrimaryKey
  = Int
  | CompositeKey

initialModel : Model
initialModel =
  { assemblages = Dict.fromList []
  , assemblies = Dict.fromList []
  , files = Dict.fromList []
  , tags = Dict.fromList []
  }

-- UPDATE

update : Celeste.ResponseTuple -> Model -> ( Model, Cmd msg )
update (assemblages, assemblies, files, tags) model =
  let
    dictifyById = Dict.fromList << List.map (\a -> (a.id, a))
    dictifyByComposite = Dict.fromList << List.map (\a -> ((a.assemblageId, a.childAssemblageId), a))
    model_ =
      { model
      | assemblages = Dict.union (dictifyById assemblages) model.assemblages
      , assemblies = Dict.union (dictifyByComposite assemblies) model.assemblies
      , files = Dict.union (dictifyById files) model.files
      , tags = Dict.union (dictifyById tags) model.tags
      }
  in (model_, Cmd.none)

-- FUNCTIONS

fetch : (Result Jwt.JwtError Celeste.Response -> msg) -> String -> Celeste.Route -> String -> Cmd msg
fetch handler apiBase route jwt =
  Jwt.send handler (Jwt.get jwt (Celeste.route apiBase route) (Celeste.decoder route))
