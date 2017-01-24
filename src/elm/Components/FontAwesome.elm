module Components.FontAwesome exposing (..)

import Html exposing (Html, i, text)
import Html.Attributes exposing (class, attribute)

fa : String -> Html msg
fa icon =
  i [ class ("fa fa-" ++ icon ++ " fa-fw"), attribute "aria-hidden" "true" ] []

faText : String -> String -> List (Html msg)
faText icon string =
  [ fa icon, text (" " ++ string) ]
