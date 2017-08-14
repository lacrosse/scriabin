module Store exposing (..)

import Data.Assemblage exposing (Assemblage)
import Data.Assembly exposing (Assembly)
import Data.File exposing (File)
import Data.Tag exposing (Tag)
import Dict exposing (Dict)
import Jwt
import Celeste
import Misc exposing (twice)


-- MODEL


type alias Store =
    { assemblages : Dict Int Assemblage
    , assemblies : Dict ( Int, Int ) Assembly
    , files : Dict Int File
    , tags : Dict Int Tag
    }


type alias Model =
    Store


type alias CompositeKey =
    ( Int, Int )


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
update ( assemblages, assemblies, files, tags ) model =
    let
        dictifyBy keyifier =
            Dict.fromList << List.map (twice ((,) << keyifier))

        model_ =
            { model
                | assemblages =
                    Dict.union
                        (dictifyBy .id assemblages)
                        model.assemblages
                , assemblies =
                    Dict.union
                        (dictifyBy (\a -> ( a.assemblageId, a.childAssemblageId )) assemblies)
                        model.assemblies
                , files =
                    Dict.union
                        (dictifyBy .id files)
                        model.files
                , tags =
                    Dict.union
                        (dictifyBy .id tags)
                        model.tags
            }
    in
        ( model_, Cmd.none )



-- FUNCTIONS


fetch :
    (Result Jwt.JwtError Celeste.Response -> msg)
    -> String
    -> Celeste.Route
    -> String
    -> Cmd msg
fetch handler endpoint cRoute jwt =
    cRoute
        |> Celeste.decoder
        |> Jwt.get jwt (Celeste.route endpoint cRoute)
        |> Jwt.send handler


assemblagesThroughAssemblies :
    Store
    -> Assemblage
    -> (Assembly -> Int)
    -> (Assembly -> Int)
    -> Data.Assembly.Kind
    -> Data.Assemblage.Kind
    -> List Assemblage
assemblagesThroughAssemblies { assemblies, assemblages } { id } foreignKey furtherForeignKey assemblyKind assemblageKind =
    assemblies
        |> Dict.filter (always (\a -> (foreignKey a) == id && (.kind a) == Just assemblyKind))
        |> Dict.values
        |> List.map furtherForeignKey
        |> List.filterMap (flip Dict.get assemblages)
        |> List.filter ((==) (Just assemblageKind) << .kind)
