module Components.FontAwesome exposing (fa)

import Html exposing (Html, i, text)
import Html.Attributes exposing (class, attribute)
import Components.Html exposing (aria)


fa : String -> Html msg
fa icon =
    i ([ class ("fa fa-" ++ icon ++ " fa-fw") ] ++ aria [ ( "hidden", "true" ) ]) []
