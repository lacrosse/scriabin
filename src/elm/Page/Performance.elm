module Page.Performance exposing (view)

import Data.Assemblage as Assemblage exposing (Assemblage)
import Data.Assembly as Assembly
import View.Assemblage
import View.Composition.Header
import View.Common
import Store exposing (Store)
import Html exposing (Html, h4, text, table, tr, td, a)
import Dict
import Messages exposing (Msg)
import I18n


view : String -> Assemblage -> I18n.Language -> Store -> List (Html Msg)
view endpoint assemblage language store =
    let
        compositions =
            Store.assemblagesThroughAssemblies
                store
                assemblage
                .childAssemblageId
                .assemblageId
                Assembly.Recorded

        inheritedHeader =
            compositions
                |> List.map (Tuple.first << View.Composition.Header.view store True)
                |> List.foldr (++) []

        performers =
            Store.assemblagesThroughAssemblies
                store
                assemblage
                .childAssemblageId
                .assemblageId
                Assembly.Performed

        generics =
            Store.assemblagesThroughAssemblies
                store
                assemblage
                .assemblageId
                .childAssemblageId
                Assembly.Generic

        mainPerformanceHeader =
            case performers of
                [] ->
                    []

                val ->
                    [ h4 [] (text "performed by " :: View.Common.enumerateLinks val) ]

        performanceHeader =
            mainPerformanceHeader ++ [ h4 [] [ text assemblage.name ] ]

        header =
            inheritedHeader ++ performanceHeader

        files =
            assemblage.fileIds
                |> List.filterMap (flip Dict.get store.files)
                |> List.sortBy .name
    in
        header
            ++ View.Assemblage.table [ text "Related" ] generics
            ++ View.Common.fileTable endpoint files
