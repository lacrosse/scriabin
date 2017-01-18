module Components.FontAwesome exposing (..)

import Html exposing (i, text)
import Html.Attributes exposing (class, attribute)

fa icon =
  i [ class ("fa fa-" ++ icon ++ " fa-fw"), attribute "aria-hidden" "true" ] []

faText icon string =
  [ fa icon, text (" " ++ string) ]
