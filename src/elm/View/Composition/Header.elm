module View.Composition.Header exposing (view)

import Data.Assemblage as Assemblage exposing (Assemblage)
import Data.Tag exposing (Tag)
import Html exposing (Html, text, h1, h3, h4, div)
import Html.Attributes exposing (class)
import Messages exposing (Msg)
import View.Common
    exposing
        ( enumerateHuman
        , assemblageLink
        , enumerateLinks
        )


view :
    Bool
    -> List Assemblage
    -> List Assemblage
    -> List Tag
    -> Assemblage
    -> ( List (Html Msg), List Tag )
view h1Link composers reconstructors tags composition =
    let
        ( creationDateTags, tags_ ) =
            List.partition ((==) "creation_date" << .key) tags

        ( tonalityTags, tags__ ) =
            List.partition ((==) "tonality" << .key) tags_

        tonality =
            if List.isEmpty tonalityTags then
                []
            else
                text " in " :: enumerateHuman (List.map (text << .value) tonalityTags)

        name =
            if h1Link then
                assemblageLink composition
            else
                (text << .name) composition

        nameHeader =
            h1 [] (name :: tonality)

        creationDate =
            case creationDateTags of
                [] ->
                    []

                val ->
                    text " in " :: enumerateHuman (List.map (text << .value) val)

        composedByHeader =
            case composers of
                [] ->
                    []

                val ->
                    [ h3 [] (text "composed by " :: enumerateLinks val ++ creationDate) ]

        reconstructedByHeader =
            case reconstructors of
                [] ->
                    []

                val ->
                    [ h4 [] (text "reconstructed by " :: enumerateLinks val) ]
    in
        ( [ div [ class "composition-header" ] (nameHeader :: composedByHeader ++ reconstructedByHeader) ], tags__ )
