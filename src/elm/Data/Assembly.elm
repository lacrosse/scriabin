module Data.Assembly exposing (..)

import Json.Decode as JD


-- MODEL


type Kind
    = Composed
    | Recorded
    | Reconstructed
    | Performed
    | Generic


type alias Assembly =
    { assemblageId : Int
    , kind : Kind
    , childAssemblageId : Int
    }



-- FUNCTIONS


parseKind : Maybe String -> Kind
parseKind maybeString =
    case maybeString of
        Just "composed" ->
            Composed

        Just "recorded" ->
            Recorded

        Just "performed" ->
            Performed

        Just "reconstructed" ->
            Reconstructed

        _ ->
            Generic



-- DECODERS


jsonDecoder : JD.Decoder Assembly
jsonDecoder =
    JD.map3 Assembly
        (JD.field "assemblage_id" JD.int)
        (JD.map parseKind (JD.field "kind" (JD.nullable JD.string)))
        (JD.field "child_assemblage_id" JD.int)
