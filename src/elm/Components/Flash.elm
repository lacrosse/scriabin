module Components.Flash exposing (..)

import Html exposing (Html, p, text)
import Html.Attributes exposing (class, attribute)

-- MODEL

type Kind
  = Info
  | Error

type alias Flash =
  (Kind, String)

type alias Model =
  Maybe Flash

initialModel : Model
initialModel =
  Nothing

-- VIEW

view : Model -> Html msg
view model =
  let
    paragraph styling string =
      p [ class ("alert alert-" ++ styling), attribute "role" "alert" ] [ text string ]
  in
    case model of
      Just (Info, string) ->
        paragraph "info" string
      Just (Error, string) ->
        paragraph "danger" string
      Nothing ->
        text ""
