module AssemblageList exposing (..)

import Html exposing (Html, div)
import Assemblage

-- MODEL

type alias Model =
  AssemblageList

type alias AssemblageList =
  List Assemblage.Model

initialModel : Model
initialModel =
  [ { name = "Alexander Scriabin" }
  , { name = "Gustav Mahler" }
  ]

-- VIEW

view : Model -> Html msg
view list =
  div [] (List.map Assemblage.view list)
