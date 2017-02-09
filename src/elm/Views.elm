module Views exposing (root, notFound, statsView,
                       tagLabel, tagsRow, navLink)

import Html exposing (Html, Attribute, span, text, small, h1, p, div, a, code)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onWithOptions)
import Json.Decode as JD

import Models.Tag exposing (Tag)
import Session exposing (User)
import Routing
import Messages

-- VIEW

root : String -> List (Html msg)
root server =
  [ h1 [] [ text "Welcome!" ]
  , p []
    [ text "I am Scriabin, a web client for Celeste. You are connected to "
    , code [] [ text server ]
    , text ". I will be your personal attendant for today."
    ]
  , p [] [ text "There is no reason to panic." ]
  ]

notFound : Html msg
notFound =
  let
    h = h1 [] [ text "Not found" ]
    par = p [] [ text "Try again." ]
  in div [] [ h, par ]

navLink : Routing.Route -> List (Attribute Messages.Msg) -> List (Html Messages.Msg) -> Html Messages.Msg
navLink route =
  let
    string = Routing.routeToString route
    options = { stopPropagation = False, preventDefault = True }
    on = onWithOptions "click" options (JD.succeed << Messages.SetRoute <| route)
  in (a << (::) (href string) << (::) on)

tagLabel : Tag -> Html msg
tagLabel { value } =
  span [ class "label label-default" ] [ text value ]

tagsRow : List Tag -> List (Html msg)
tagsRow tags =
  if List.isEmpty tags then
    []
  else
    [small [] (text "tags: " :: List.map tagLabel tags)]

statsView : a -> Html msg
statsView user =
  text "hi"
