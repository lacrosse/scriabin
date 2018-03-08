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
        joinAssemblagesThrough =
            Store.assemblagesThroughAssemblies
                store
                assemblage

        parentAssemblagesThroughAssemblies =
            joinAssemblagesThrough .childAssemblageId .assemblageId

        childrenAssemblagesThroughAssemblies =
            joinAssemblagesThrough .assemblageId .childAssemblageId

        composers =
            parentAssemblagesThroughAssemblies Assembly.Composed

        reconstructors =
            parentAssemblagesThroughAssemblies Assembly.Reconstructed

        files =
            assemblage.fileIds
                |> List.filterMap (flip Dict.get store.files)
                |> List.sortBy .name

        childrenCompositions =
            childrenAssemblagesThroughAssemblies Assembly.Composed

        childrenPerformances =
            childrenAssemblagesThroughAssemblies Assembly.Performed

        reconstructions =
            childrenAssemblagesThroughAssemblies Assembly.Reconstructed

        tagsFor a =
            List.filterMap (flip Dict.get store.tags) a.tagIds

        tags =
            tagsFor assemblage

        parentCompositions =
            parentAssemblagesThroughAssemblies Assembly.EmbodiedBy

        parentCompositionsWithTags =
            parentCompositions
                |> List.map (\composition -> ( composition, tagsFor composition ))

        embodiments =
            childrenAssemblagesThroughAssemblies Assembly.EmbodiedBy

        performers =
            parentAssemblagesThroughAssemblies Assembly.Performed

        generics =
            childrenAssemblagesThroughAssemblies Assembly.Generic
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
