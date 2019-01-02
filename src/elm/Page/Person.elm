module Page.Person exposing (view)

import I18n exposing (t)
import Html exposing (Html, h1, text)
import Messages exposing (Msg)
import Data.Assemblage as Assemblage exposing (Assemblage)
import Data.File as File exposing (File)
import View.Common
import View.Assemblage
import Connection.Server.Types exposing (Endpoint)


view :
    List Assemblage
    -> List Assemblage
    -> List Assemblage
    -> List File
    -> Assemblage
    -> I18n.Language
    -> Endpoint
    -> List (Html Msg)
view compositions reconstructions performances files assemblage language endpoint =
    let
        header =
            [ h1 [] [ text assemblage.name ] ]

        sortedPerformances =
            List.sortBy .name performances

        sortedCompositions =
            List.sortBy .name compositions
    in
        header
            ++ (View.Common.fileTable endpoint files)
            ++ (View.Assemblage.table (t language I18n.Performances) sortedPerformances)
            ++ (View.Assemblage.table (t language I18n.Compositions) sortedCompositions)
            ++ (View.Assemblage.table [ text "Reconstructions of other composers' works" ] reconstructions)
