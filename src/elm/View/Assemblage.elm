module View.Assemblage exposing (..)

import Html exposing (Html, thead, tbody, tr, th, td)
import Html.Attributes exposing (class)
import View.Common
import Data.Assemblage exposing (Assemblage)
import Messages exposing (Msg)


row : Assemblage -> Html Msg
row a =
    tr [] [ td [] [ View.Common.assemblageLink a ] ]


table : List (Html Msg) -> List Assemblage -> List (Html Msg)
table name assemblages =
    if List.isEmpty assemblages then
        []
    else
        [ Html.table [ class "table" ]
            [ thead [] [ tr [] [ th [] name ] ]
            , tbody [] (List.map row assemblages)
            ]
        ]
