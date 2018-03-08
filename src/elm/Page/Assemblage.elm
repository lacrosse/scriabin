module Page.Assemblage exposing (..)

import Html exposing (Html)
import I18n
import Messages exposing (Msg)
import Data.Assemblage as Assemblage exposing (Assemblage)
import Data.Assembly as Assembly exposing (Assembly)
import Page.Person
import Page.Composition
import Page.Performance
import Store exposing (Store)
import Dict


view : Assemblage -> Store -> I18n.Language -> String -> List (Html Msg)
view assemblage store =
    let
        composers =
            Store.assemblagesThroughAssemblies
                store
                assemblage
                .childAssemblageId
                .assemblageId
                Assembly.Composed

        reconstructors =
            Store.assemblagesThroughAssemblies
                store
                assemblage
                .childAssemblageId
                .assemblageId
                Assembly.Reconstructed

        files =
            assemblage.fileIds
                |> List.filterMap (flip Dict.get store.files)
                |> List.sortBy .name

        childrenCompositions =
            Store.assemblagesThroughAssemblies
                store
                assemblage
                .assemblageId
                .childAssemblageId
                Assembly.Composed

        childrenPerformances =
            Store.assemblagesThroughAssemblies
                store
                assemblage
                .assemblageId
                .childAssemblageId
                Assembly.Performed

        reconstructions =
            Store.assemblagesThroughAssemblies
                store
                assemblage
                .assemblageId
                .childAssemblageId
                Assembly.Reconstructed

        tagsFor a =
            List.filterMap (flip Dict.get store.tags) a.tagIds

        tags =
            tagsFor assemblage

        parentCompositions =
            Store.assemblagesThroughAssemblies
                store
                assemblage
                .childAssemblageId
                .assemblageId
                Assembly.EmbodiedBy

        parentCompositionsWithTags =
            parentCompositions
                |> List.map (\composition -> ( composition, tagsFor composition ))

        embodiments =
            Store.assemblagesThroughAssemblies
                store
                assemblage
                .assemblageId
                .childAssemblageId
                Assembly.EmbodiedBy

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
    in
        case assemblage.kind of
            Assemblage.Person ->
                Page.Person.view childrenCompositions reconstructions childrenPerformances files assemblage

            Assemblage.Composition ->
                Page.Composition.view composers reconstructors tags embodiments assemblage

            Assemblage.Recording ->
                Page.Performance.view assemblage composers reconstructors parentCompositionsWithTags performers generics files

            Assemblage.Generic ->
                Page.Person.view childrenCompositions reconstructions childrenPerformances files assemblage
