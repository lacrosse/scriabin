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
    old = model
    dictifyById = Dict.fromList << List.map (\a -> (a.id, a))
    dictifyByComposite = Dict.fromList << List.map (\a -> ((a.assemblageId, a.childAssemblageId), a))
    assemblagesDict = dictifyById assemblages
    assembliesDict = dictifyByComposite assemblies
    filesDict = dictifyById files
    tagsDict = dictifyById tags
    newAssemblages = Dict.union assemblagesDict old.assemblages
    newAssemblies = Dict.union assembliesDict old.assemblies
    newFiles = Dict.union filesDict old.files
    newTags = Dict.union tagsDict old.tags
    new = { old | assemblages = newAssemblages, assemblies = newAssemblies, files = newFiles, tags = newTags }
  in (new, Cmd.none)

-- FUNCTIONS

fetch : (Result Jwt.JwtError Celeste.Response -> msg) -> String -> Celeste.Route -> String -> Cmd msg
fetch handler apiBase route jwt =
  Jwt.send handler (Jwt.get jwt (Celeste.route apiBase route) (Celeste.decoder route))
