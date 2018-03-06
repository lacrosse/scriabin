module Celeste exposing (..)

import Json.Decode as JD
import Data.Account
import Data.Assemblage exposing (Assemblage)
import Data.Assembly exposing (Assembly)
import Data.File exposing (File)
import Data.Tag exposing (Tag)
import Jwt
import Http


-- MODEL


type Route
    = Composers
    | Assemblage Int
    | Account


type Response
    = ComposersResponse (List Assemblage)
    | AssemblageResponse ( Assemblage, List Assemblage, List Assembly, List File, List Tag )
    | AccountResponse String


type alias ResponseResult =
    Result Jwt.JwtError Response


type alias ResponseTuple =
    ( List Assemblage, List Assembly, List File, List Tag )


type alias Outcome =
    Result String ResponseTuple



-- FUNCTIONS


url : String -> Route -> String
url endpoint route =
    let
        relative =
            case route of
                Composers ->
                    "/composers"

                Assemblage id ->
                    "/assemblages/" ++ (toString id)

                Account ->
                    "/account"
    in
        endpoint ++ relative


decoderForRoute : Route -> JD.Decoder Response
decoderForRoute route =
    case route of
        Composers ->
            (JD.map ComposersResponse << JD.field "assemblages" << JD.list) Data.Assemblage.jsonDecoder

        Assemblage id ->
            JD.map AssemblageResponse <|
                JD.map5 (,,,,)
                    (JD.field "assemblage" Data.Assemblage.jsonDecoder)
                    (JD.field "assemblages" (JD.list Data.Assemblage.jsonDecoder))
                    (JD.field "assemblies" (JD.list Data.Assembly.jsonDecoder))
                    (JD.field "files" (JD.list Data.File.jsonDecoder))
                    (JD.field "tags" (JD.list Data.Tag.jsonDecoder))

        Account ->
            JD.map AccountResponse <| Data.Account.jsonDecoder


responseToTuple : Response -> ResponseTuple
responseToTuple resp =
    case resp of
        AssemblageResponse ( assemblage, assemblages, assemblies, files, tags ) ->
            ( assemblage :: assemblages, assemblies, files, tags )

        ComposersResponse assemblages ->
            ( assemblages, [], [], [] )

        _ ->
            ( [], [], [], [] )


resultErrToString : Jwt.JwtError -> String
resultErrToString err =
    case err of
        Jwt.HttpError (Http.BadPayload val _) ->
            val

        Jwt.HttpError (Http.NetworkError) ->
            "I’ve encountered a network error."

        error ->
            toString error


resultToOutcome : ResponseResult -> Outcome
resultToOutcome celesteResult =
    celesteResult
        |> Result.map responseToTuple
        |> Result.mapError resultErrToString
