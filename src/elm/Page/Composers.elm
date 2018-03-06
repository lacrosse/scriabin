module Page.Composers exposing (view)

import Html exposing (Html, text)
import Messages exposing (Msg)
import Data.Assemblage
import View.Assemblage
import Store
import Dict


view : Store.Model -> List (Html Msg)
view store =
    let
        composers =
            store.assemblages
                |> Dict.filter (always Data.Assemblage.isComposer)
                |> Dict.toList
                |> List.map Tuple.second
                |> List.sortBy .name
    in
        View.Assemblage.table [ text "Composers" ] composers
