module Data.Assemblage exposing (..)

import Json.Decode as JD
import Data.File exposing (File)
import Regex


-- MODEL


type Kind
    = Person
    | Composition
    | Recording
    | Generic


type alias Assemblage =
    { id : Int
    , name : String
    , kind : Kind
    , fileIds : List Int
    , tagIds : List Int
    }



-- FUNCTIONS


isComposer : Assemblage -> Bool
isComposer { kind } =
    case kind of
        Person ->
            True

        _ ->
            False


parseKind : Maybe String -> Kind
parseKind maybeString =
    case maybeString of
        Just "person" ->
            Person

        Just "composition" ->
            Composition

        Just "recording" ->
            Recording

        _ ->
            Generic


toUrlSlug : Assemblage -> String
toUrlSlug { name } =
    "-"
        ++ (name
                |> Regex.replace Regex.All (Regex.regex "\\W+") (always "-")
                |> Regex.replace Regex.All (Regex.regex "-+$") (always "")
                |> Regex.replace Regex.All (Regex.regex "^-+") (always "")
                |> String.toLower
           )


childrenFiles : Assemblage -> List File
childrenFiles assemblage =
    []



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
            (JD.map parseKind (JD.field "kind" (JD.nullable JD.string)))
            (maybeList "file_ids" JD.int)
            (maybeList "tag_ids" JD.int)
