module Celeste exposing (..)

import Json.Decode as JD
import Models.Account
import Models.Assemblage
import Models.Assembly
import Models.File
import Models.Tag

-- MODEL

type Route
  = Composers
  | Assemblage Int
  | Account

type Response
  = ComposersResponse (List (Models.Assemblage.Assemblage))
  | AssemblageResponse
    ( Models.Assemblage.Assemblage
    , List Models.Assemblage.Assemblage
    , List Models.Assembly.Assembly
    , List Models.File.File
    , List Models.Tag.Tag
    )
  | AccountResponse (String)

type alias ResponseTuple =
  ( List Models.Assemblage.Assemblage
  , List Models.Assembly.Assembly
  , List Models.File.File
  , List Models.Tag.Tag
  )

-- FUNCTIONS

route : String -> Route -> String
route apiBase route =
  let
    rel =
      case route of
        Composers -> "/composers"
        Assemblage id -> "/assemblages/" ++ (toString id)
        Account -> "/account"
  in apiBase ++ rel

decoder : Route -> JD.Decoder Response
decoder route =
  case route of
    Composers ->
      (JD.map ComposersResponse << JD.field "assemblages" << JD.list) Models.Assemblage.jsonDecoder
    Assemblage id ->
      let
        tupleDecoder =
          JD.map5 (,,,,)
            (JD.field "assemblage" Models.Assemblage.jsonDecoder)
            (JD.field "assemblages" (JD.list Models.Assemblage.jsonDecoder))
            (JD.field "assemblies" (JD.list Models.Assembly.jsonDecoder))
            (JD.field "files" (JD.list Models.File.jsonDecoder))
            (JD.field "tags" (JD.list Models.Tag.jsonDecoder))
      in JD.map AssemblageResponse tupleDecoder
    Account ->
      JD.map AccountResponse <| Models.Account.jsonDecoder

responseToTuple : Response -> ResponseTuple
responseToTuple resp =
  case resp of
    AssemblageResponse (assemblage, assemblages, assemblies, files, tags) ->
      (assemblage :: assemblages, assemblies, files, tags)
    ComposersResponse assemblages ->
      (assemblages, [], [], [])
    _ ->
      ([], [], [], [])
