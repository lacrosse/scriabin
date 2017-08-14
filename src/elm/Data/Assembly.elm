module Data.Assembly exposing (..)

import Json.Decode as JD


-- MODEL


type Kind
    = Composed
    | Recorded
    | Reconstructed
    | Performed


type alias Assembly =
    { assemblageId : Int
    , kind : Maybe Kind
    , childAssemblageId : Int
    }



-- FUNCTIONS


parseKind : String -> Maybe Kind
parseKind string =
    case string of
        "composed" ->
            Just Composed

        "recorded" ->
            Just Recorded

        "performed" ->
            Just Performed

        "reconstructed" ->
            Just Reconstructed

        _ ->
            Nothing



-- DECODERS


jsonDecoder : JD.Decoder Assembly
jsonDecoder =
    JD.map3 Assembly
        (JD.field "assemblage_id" JD.int)
        (JD.map parseKind (JD.field "kind" JD.string))
        (JD.field "child_assemblage_id" JD.int)
