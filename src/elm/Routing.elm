module Routing exposing (..)

import Navigation exposing (Location)
import UrlParser exposing (Parser, parseHash, oneOf, top, map, s, (</>), int)

-- MODEL

type Route
  = Root
  | NewSession
  | Composers
  | Assemblage Int

type alias Model =
  { currentRoute : Maybe Route }

initialModel : Model
initialModel =
  { currentRoute = Nothing }

-- FUNCTIONS

routingTable : List (Parser (Route -> c) c)
routingTable =
  [ map Assemblage (s "assemblages" </> int)
  , map NewSession (s "sign-in")
  , map Composers  (s "composers")
  , map Root       (top)
  ]

locationToRoute : Location -> Maybe Route
locationToRoute = parseHash (oneOf routingTable)

routeToString : Route -> String
routeToString route =
  case route of
    Root ->
      "/"
    NewSession ->
      "#/sign-in"
    Composers ->
      "#/composers"
    Assemblage id ->
      "#/assemblages/" ++ (toString id)
