module Page.Performance exposing (view)

import Data.Assemblage as Assemblage exposing (Assemblage)
import Data.Assembly as Assembly
import View.Composition.Header
import View.Common
    exposing
        ( fileTable
        )
import Store exposing (Store, assemblagesThroughAssemblies)
import Html exposing (Html, h4, text, table, tr, td, a)
import Dict
import Messages exposing (Msg)
import I18n


view : Assemblage -> I18n.Language -> Store -> List (Html Msg)
view assemblage language store =
    let
        compositions =
            assemblagesThroughAssemblies store
                assemblage
                .childAssemblageId
                .assemblageId
                Assembly.Recorded
                Assemblage.Composition

        inheritedHeader =
            compositions
                |> List.map (Tuple.first << View.Composition.Header.view store True)
                |> List.foldr (++) []

        performers =
            assemblagesThroughAssemblies
                store
                assemblage
                .childAssemblageId
                .assemblageId
                Assembly.Performed
                Assemblage.Person

        mainPerformanceHeader =
            case performers of
                [] ->
                    []

                val ->
                    [ h4 [] (text "performed by " :: View.Common.enumerateLinks val) ]

        performanceHeader =
            mainPerformanceHeader ++ [ h4 [] [ text assemblage.name ] ]

        files =
            assemblage.fileIds
                |> List.filterMap (flip Dict.get store.files)
                |> List.sortBy .name
    in
        inheritedHeader ++ performanceHeader ++ fileTable files
