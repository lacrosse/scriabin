module Page.Performance exposing (view)

import Html exposing (Html, h4, text, table, tr, td, a)
import Data.Assemblage as Assemblage exposing (Assemblage)
import Data.Tag as Tag exposing (Tag)
import Data.File exposing (File)
import View.Assemblage
import View.Composition.Header
import View.Common
import Messages exposing (Msg)
import I18n


view :
    Assemblage
    -> List ( Assemblage, List Assemblage, List Assemblage, List Tag )
    -> List Assemblage
    -> List Assemblage
    -> List File
    -> I18n.Language
    -> String
    -> List (Html Msg)
view performance compositionsWithComposersReconstructorsAndTags performers generics files language endpoint =
    let
        inheritedHeader =
            compositionsWithComposersReconstructorsAndTags
                |> List.map (\( composition, composers, reconstructors, tags ) -> (Tuple.first << View.Composition.Header.view True composers reconstructors tags) composition)
                |> List.foldr (++) []

        mainPerformanceHeader =
            case performers of
                [] ->
                    []

                val ->
                    [ h4 [] (text "performed by " :: View.Common.enumerateLinks val) ]

        performanceHeader =
            mainPerformanceHeader ++ [ h4 [] [ text performance.name ] ]

        header =
            inheritedHeader ++ performanceHeader
    in
        header
            ++ View.Assemblage.table [ text "Related" ] generics
            ++ View.Common.fileTable endpoint files
