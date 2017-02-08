module Routing exposing (..)

import Navigation exposing (Location)
import UrlParser exposing (Parser, parseHash, oneOf, top, map, s, (</>), int, custom)
import Regex

-- MODEL

type Route
  = Root
  | NewSession
  | Composers
  | Assemblage Int String

type alias Model =
  { currentRoute : Maybe Route }

initialModel : Model
initialModel =
  { currentRoute = Nothing }

-- FUNCTIONS

basedInt : Parser (Int -> b) b
basedInt =
  custom "BASED_INT" <| \s ->
    (String.toInt << Maybe.withDefault s << List.head << Regex.split (Regex.AtMost 1) (Regex.regex "-")) s

routingTable : List (Parser (Route -> c) c)
routingTable =
  [ map (flip Assemblage "") (s "assemblages" </> basedInt)
  , map NewSession           (s "sign-in")
  , map Composers            (s "composers")
  , map Root                 (top)
  ]

locationToRoute : Location -> Maybe Route
locationToRoute = parseHash (oneOf routingTable)

routeToString : Route -> String
routeToString route =
  case route of
    Root ->
      "#/"
    NewSession ->
      "#/sign-in"
    Composers ->
      "#/composers"
    Assemblage id string ->
      "#/assemblages/" ++ toString id ++ string
