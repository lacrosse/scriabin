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


view : Assemblage -> Store -> I18n.Language -> String -> List (Html Msg)
view assemblage store =
    let
        joinAssemblagesThrough =
            assemblage
                |> Store.assemblagesThroughAssemblies store

        parentAssemblages =
            joinAssemblagesThrough ( .childAssemblageId, .assemblageId )

        childrenAssemblages =
            joinAssemblagesThrough ( .assemblageId, .childAssemblageId )

        composers =
            parentAssemblages Assembly.Composed

        reconstructors =
            parentAssemblages Assembly.Reconstructed

        files =
            assemblage
                |> Store.project store ( .fileIds, .files )
                |> List.sortBy .name

        childrenCompositions =
            childrenAssemblages Assembly.Composed

        childrenPerformances =
            childrenAssemblages Assembly.Performed

        reconstructions =
            childrenAssemblages Assembly.Reconstructed

        tagsFor assemblage =
            assemblage
                |> Store.project store ( .tagIds, .tags )

        tags =
            tagsFor assemblage

        parentCompositions =
            parentAssemblages Assembly.EmbodiedBy

        parentCompositionsWithTags =
            parentCompositions
                |> List.map (\composition -> ( composition, tagsFor composition ))

        embodiments =
            childrenAssemblages Assembly.EmbodiedBy

        performers =
            parentAssemblages Assembly.Performed

        generics =
            childrenAssemblages Assembly.Generic
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
