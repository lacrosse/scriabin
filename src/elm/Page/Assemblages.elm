module Page.Assemblages exposing (view)

import Html
    exposing
        ( Html
        , div
        , p
        )
import Data.Assemblage exposing (Assemblage)
import Messages exposing (Msg)
import View.Common


assemblageRowView : Assemblage -> Html Msg
assemblageRowView =
    p [] << List.singleton << View.Common.assemblageLink


view : List Assemblage -> List (Html Msg)
view =
    flip (::) [] << div [] << List.map assemblageRowView
