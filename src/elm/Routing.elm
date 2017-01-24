module Routing exposing (..)

import Navigation exposing (Location)
import Html exposing (Html, Attribute, a)
import Html.Attributes exposing (href)
import Html.Events exposing (onWithOptions)
import UrlParser exposing (Parser, parseHash, oneOf, top, map, s, (</>), int)
import Json.Decode

-- MODEL

type Route
  = Root
  | NewSession
  | Composers
  | Assemblage Int

type alias Model =
  { currentRoute : Maybe Route }

initialModel : Location -> Model
initialModel navLoc =
  { currentRoute = locationToRoute navLoc }

-- UPDATE

type Msg
  = SetRoute Route
  | VisitLocation Location

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    SetRoute route ->
      ( model, Navigation.newUrl (routeToString route) )
    VisitLocation navLoc ->
      ( { model | currentRoute = locationToRoute navLoc }, Cmd.none )

-- VIEW

navLink : (Msg -> msg) -> Route -> List (Attribute msg) -> List (Html msg) -> Html msg
navLink msgWrapper route attributes =
  let
    string = routeToString route
    options = { stopPropagation = True, preventDefault = True }
    visitAndDecode = Json.Decode.succeed << msgWrapper << SetRoute
    on = onWithOptions "click" options (visitAndDecode route)
  in a (href string :: on :: attributes)

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
