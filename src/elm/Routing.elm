module Routing exposing (..)

import Navigation

-- MODEL

type Page
  = Root
  | NewSession
  | NewAssemblage
  | Assemblages
  | Composers

type alias Model =
  { currentPage : Maybe Page }

initialModel : Navigation.Location -> Model
initialModel navLoc =
  { currentPage = navLocationToPage navLoc }

-- UPDATE

type Msg
  = VisitPage Page
  | UpdatePage Navigation.Location

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    VisitPage page ->
      ( model, Navigation.newUrl (pageToString page) )
    UpdatePage navLoc ->
      ( { model | currentPage = navLocationToPage navLoc }
      , Cmd.none
      )

-- FUNCTIONS

navLocationToPage : Navigation.Location -> Maybe Page
navLocationToPage { hash } =
  stringToPage hash

stringToPage : String -> Maybe Page
stringToPage string =
  case string of
    "" ->
      Just Root
    "#/" ->
      Just Root
    "#/sign-in" ->
      Just NewSession
    "#/assemblages" ->
      Just Assemblages
    "#/assemblages/new" ->
      Just NewAssemblage
    "#/composers" ->
      Just Composers
    _ ->
      Nothing

-- Deprecated.
pageToString : Page -> String
pageToString page =
  case page of
    Root ->
      "#/"
    NewSession ->
      "#/sign-in"
    Assemblages ->
      "#/assemblages"
    NewAssemblage ->
      pageToString (Assemblages) ++ "/new"
    Composers ->
      "#/composers"
