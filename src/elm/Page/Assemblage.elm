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
        joinAssemblagesThrough assemblage =
            Store.assemblagesThroughAssemblies store assemblage

        parentAssemblages assemblage =
            joinAssemblagesThrough assemblage ( .childAssemblageId, .assemblageId )

        childrenAssemblages assemblage =
            joinAssemblagesThrough assemblage ( .assemblageId, .childAssemblageId )

        composersFor assemblage =
            parentAssemblages assemblage Assembly.Composed

        reconstructorsFor assemblage =
            parentAssemblages assemblage Assembly.Reconstructed

        tagsFor assemblage =
            assemblage
                |> Store.project store ( .tagIds, .tags )

        parentComposers =
            composersFor assemblage

        reconstructors =
            reconstructorsFor assemblage

        files =
            assemblage
                |> Store.project store ( .fileIds, .files )
                |> List.sortBy .name

        childrenCompositions =
            childrenAssemblages assemblage Assembly.Composed

        childrenPerformances =
            childrenAssemblages assemblage Assembly.Performed

        reconstructions =
            childrenAssemblages assemblage Assembly.Reconstructed

        parentCompositions =
            parentAssemblages assemblage Assembly.EmbodiedBy

        embodiments =
            childrenAssemblages assemblage Assembly.EmbodiedBy

        performers =
            parentAssemblages assemblage Assembly.Performed

        generics =
            childrenAssemblages assemblage Assembly.Generic
    in
        case assemblage.kind of
            Assemblage.Person ->
                Page.Person.view childrenCompositions reconstructions childrenPerformances files assemblage

            Assemblage.Composition ->
                Page.Composition.view
                    parentComposers
                    reconstructors
                    (tagsFor assemblage)
                    embodiments
                    assemblage

            Assemblage.Performance ->
                Page.Performance.view
                    assemblage
                    (List.map (\c -> ( c, composersFor c, reconstructorsFor c, tagsFor c )) parentCompositions)
                    performers
                    generics
                    files

            Assemblage.Generic ->
                Page.Person.view childrenCompositions reconstructions childrenPerformances files assemblage
