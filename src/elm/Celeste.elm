module Celeste exposing (..)

import Json.Decode as JD
import Models.Assemblage
import Models.Assembly
import Models.File

-- MODEL

type Route
  = Composers
  | Assemblage Int

type Response
  = ComposersResponse (List (Models.Assemblage.Assemblage))
  | AssemblageResponse
    ( Models.Assemblage.Assemblage
    , List Models.Assemblage.Assemblage
    , List Models.Assembly.Assembly
    , List Models.File.File
    )

type alias ResponseTuple =
  (List Models.Assemblage.Assemblage, List Models.Assembly.Assembly, List Models.File.File)

-- FUNCTIONS

route : String -> Route -> String
route apiBase route =
  let
    rel =
      case route of
        Composers -> "/composers"
        Assemblage id -> "/assemblages/" ++ (toString id)
  in apiBase ++ rel

decoder : Route -> JD.Decoder Response
decoder route =
  case route of
    Composers ->
      (JD.map ComposersResponse << JD.field "assemblages" << JD.list) Models.Assemblage.jsonDecoder
    Assemblage id ->
      let
        tupleDecoder =
          JD.map4 (,,,)
            (JD.field "assemblage" Models.Assemblage.jsonDecoder)
            (JD.field "assemblages" (JD.list Models.Assemblage.jsonDecoder))
            (JD.field "assemblies" (JD.list Models.Assembly.jsonDecoder))
            (JD.field "files" (JD.list Models.File.jsonDecoder))
      in JD.map AssemblageResponse tupleDecoder

responseToTuple : Response -> ResponseTuple
responseToTuple resp =
  case resp of
    AssemblageResponse (assemblage, assemblages, assemblies, files) ->
      (assemblage :: assemblages, assemblies, files)
    ComposersResponse assemblages ->
      (assemblages, [], [])
