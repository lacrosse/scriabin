module Celeste exposing (..)

import Json.Decode as JD
import Data.Account
import Data.Assemblage
import Data.Assembly
import Data.File
import Data.Tag

-- MODEL

type Route
  = Composers
  | Assemblage Int
  | Account

type Response
  = ComposersResponse (List (Data.Assemblage.Assemblage))
  | AssemblageResponse
    ( Data.Assemblage.Assemblage
    , List Data.Assemblage.Assemblage
    , List Data.Assembly.Assembly
    , List Data.File.File
    , List Data.Tag.Tag
    )
  | AccountResponse (String)

type alias ResponseTuple =
  ( List Data.Assemblage.Assemblage
  , List Data.Assembly.Assembly
  , List Data.File.File
  , List Data.Tag.Tag
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
      (JD.map ComposersResponse << JD.field "assemblages" << JD.list) Data.Assemblage.jsonDecoder
    Assemblage id ->
      let
        tupleDecoder =
          JD.map5 (,,,,)
            (JD.field "assemblage" Data.Assemblage.jsonDecoder)
            (JD.field "assemblages" (JD.list Data.Assemblage.jsonDecoder))
            (JD.field "assemblies" (JD.list Data.Assembly.jsonDecoder))
            (JD.field "files" (JD.list Data.File.jsonDecoder))
            (JD.field "tags" (JD.list Data.Tag.jsonDecoder))
      in JD.map AssemblageResponse tupleDecoder
    Account ->
      JD.map AccountResponse <| Data.Account.jsonDecoder

responseToTuple : Response -> ResponseTuple
responseToTuple resp =
  case resp of
    AssemblageResponse (assemblage, assemblages, assemblies, files, tags) ->
      (assemblage :: assemblages, assemblies, files, tags)
    ComposersResponse assemblages ->
      (assemblages, [], [], [])
    _ ->
      ([], [], [], [])
