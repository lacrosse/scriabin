module View.Composition.Header exposing (view)

import Data.Assembly as Assembly exposing (Assembly)
import Data.Assemblage as Assemblage exposing (Assemblage)
import Data.Tag exposing (Tag)
import Html exposing (Html, text, h1, h3, h4, div)
import Html.Attributes exposing (class)
import Messages exposing (Msg)
import Store exposing (Store, assemblagesThroughAssemblies)
import View.Common
    exposing
        ( enumerateHuman
        , assemblageLink
        , enumerateLinks
        )
import Dict


view : Store -> Bool -> Assemblage -> ( List (Html Msg), List Tag )
view store h1Link assemblage =
    let
        allTags =
            List.filterMap (flip Dict.get store.tags) assemblage.tagIds

        ( creationDateTags, tags_ ) =
            List.partition ((==) "creation_date" << .key) allTags

        ( tonalityTags, tags ) =
            List.partition ((==) "tonality" << .key) tags_

        tonality =
            if List.isEmpty tonalityTags then
                []
            else
                text " in " :: enumerateHuman (List.map (text << .value) tonalityTags)

        name =
            if h1Link then
                assemblageLink assemblage
            else
                (text << .name) assemblage

        nameHeader =
            h1 [] (name :: tonality)

        composers =
            assemblagesThroughAssemblies
                store
                assemblage
                .childAssemblageId
                .assemblageId
                Assembly.Composed

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

        reconstructors =
            assemblagesThroughAssemblies
                store
                assemblage
                .childAssemblageId
                .assemblageId
                Assembly.Reconstructed

        reconstructedByHeader =
            case reconstructors of
                [] ->
                    []

                val ->
                    [ h4 [] (text "reconstructed by " :: enumerateLinks val) ]
    in
        ( [ div [ class "composition-header" ] (nameHeader :: composedByHeader ++ reconstructedByHeader) ], tags )
