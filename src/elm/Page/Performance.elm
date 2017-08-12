module Page.Performance exposing (view)

import Data.Assemblage as Assemblage exposing (Assemblage)
import Data.Assembly as Assembly
import Data.File as File exposing (File)
import View.Composition.Header
import View.Common
    exposing
        ( prependAndEnumerateLinks
        , fileTable
        )
import Store exposing (Store, assemblagesThroughAssemblies)
import Components.Player as Player
import Components.FontAwesome exposing (fa)
import Html exposing (Html, h4, text, table, tr, td, a)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onWithOptions)
import Dict
import Json.Decode as JD
import Components.Player
import Messages exposing (Msg)


view : Assemblage -> Store -> List (Html Msg)
view assemblage store =
    let
        compositions =
            assemblagesThroughAssemblies store assemblage .childAssemblageId .assemblageId Assembly.Recorded Assemblage.Composition

        inheritedHeader =
            compositions
                |> List.map (Tuple.first << View.Composition.Header.view store True)
                |> List.foldr (++) []

        performers =
            assemblagesThroughAssemblies store assemblage .childAssemblageId .assemblageId Assembly.Performed Assemblage.Person

        mainPerformanceHeader =
            if List.isEmpty performers then
                []
            else
                [ h4 [] ([ text "performed " ] ++ prependAndEnumerateLinks "by" performers) ]

        performanceHeader =
            mainPerformanceHeader ++ [ h4 [] [ text assemblage.name ] ]

        files =
            assemblage.fileIds
                |> List.filterMap (flip Dict.get store.files)
                |> List.sortBy .name
    in
        inheritedHeader ++ performanceHeader ++ (fileTable files)
