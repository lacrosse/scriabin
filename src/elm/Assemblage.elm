module Assemblage exposing (..)

import Html exposing (Html, p, text)

-- MODEL

type alias Model =
  Assemblage

type alias Assemblage =
  { name : String
  }

-- VIEW

view : Model -> Html msg
view assemblage =
  p [] [
    text assemblage.name
  ]
