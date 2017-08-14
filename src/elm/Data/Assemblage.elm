module Data.Assemblage exposing (..)

import Json.Decode as JD
import Regex


-- MODEL


type Kind
    = Person
    | Composition
    | Recording


type alias Assemblage =
    { id : Int
    , name : String
    , kind : Maybe Kind
    , fileIds : List Int
    , tagIds : List Int
    }



-- FUNCTIONS


isComposer : Assemblage -> Bool
isComposer { kind } =
    case kind of
        Just Person ->
            True

        _ ->
            False


parseKind : String -> Maybe Kind
parseKind string =
    case string of
        "person" ->
            Just Person

        "composition" ->
            Just Composition

        "recording" ->
            Just Recording

        _ ->
            Nothing


toUrlSlug : Assemblage -> String
toUrlSlug { name } =
    "-"
        ++ name
        |> Regex.replace Regex.All (Regex.regex "\\W+") (always "-")
        |> Regex.replace Regex.All (Regex.regex "-+$") (always "")
        |> String.toLower



-- DECODERS


jsonDecoder : JD.Decoder Assemblage
jsonDecoder =
    let
        maybeList key =
            JD.map (Maybe.withDefault []) << JD.maybe << JD.field key << JD.list
    in
        JD.map5 Assemblage
            (JD.field "id" JD.int)
            (JD.field "name" JD.string)
            ((JD.map parseKind << JD.field "kind") JD.string)
            (maybeList "file_ids" JD.int)
            (maybeList "tag_ids" JD.int)
