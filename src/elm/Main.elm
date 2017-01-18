module Main exposing (..)

import Html exposing (Html, Attribute, text, a, div, span, button, ul, li, main_, nav)
import Html.Attributes exposing (class, type_, attribute, href)
import Html.Events exposing (onClick, onWithOptions)
import Json.Decode
import Components.FontAwesome exposing (faText)
import Session
import AssemblageList
import Components.Flash as Flash
import Navigation
import Routing

-- MODEL

type alias Model =
  { currentPage : Routing.Model
  , flash : Flash.Model
  , session : Session.Model
  , assemblages : AssemblageList.Model
  }

initialModel : Navigation.Location -> Model
initialModel navLocation =
  { currentPage = Routing.initialModel navLocation
  , assemblages = AssemblageList.initialModel
  , session = Session.initialModel
  , flash = Flash.initialModel
  }

init : Navigation.Location -> (Model, Cmd Msg)
init navLocation =
  ( initialModel navLocation
  , Cmd.none
  )

-- UPDATE

type Msg
  = Noop
  | SessionMsg Session.Msg
  | RoutingMsg Routing.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Noop ->
      ( model
      , Cmd.none
      )
    SessionMsg msg ->
      let
        (updatedSession, cmd) = Session.update msg model.session
      in
        ( { model | session = updatedSession }
        , Cmd.map SessionMsg cmd
        )
    RoutingMsg msg ->
      let
        (updatedCurrentPage, cmd) = Routing.update msg model.currentPage
      in
        ( { model | currentPage = updatedCurrentPage }
        , Cmd.map RoutingMsg cmd
        )

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

navLink : Routing.Page -> List (Attribute Msg) -> List (Html Msg) -> Html Msg
navLink page attributes =
  let
    string = Routing.pageToString page
    options = { stopPropagation = True, preventDefault = True }
    decode = (Json.Decode.succeed (RoutingMsg (Routing.VisitPage page)))
    on = onWithOptions "click" options decode
  in a (href string :: on :: attributes)

template : Model -> Html Msg
template model =
  case model.currentPage of
    Just Routing.Root ->
      text "Hi."
    Just Routing.NewSession ->
      text "Sign in pläse"
    Just Routing.Composers ->
      AssemblageList.view model.assemblages
    Just Routing.Assemblages ->
      text "Assemblages"
    Just Routing.NewAssemblage ->
      text "New assemblage"
    Nothing ->
      text "404"

view : Model -> Html Msg
view model =
  let
    navbarHeader =
      div [ class "navbar-header" ]
        [ button
          [ type_ "button"
          , class "navbar-toggle collapsed"
          , attribute "data-toggle" "collapse"
          , attribute "data-target" "#bs-example-navbar-collapse-1"
          , attribute "aria-expanded" "false"
          ]
          [ span [ class "sr-only" ] [ text "Toggle navigation" ]
          , span [ class "icon-bar" ] []
          , span [ class "icon-bar" ] []
          , span [ class "icon-bar" ] []
          ]
        , navLink Routing.Root [ class "navbar-brand" ] (faText "music" "Celeste")
        ]
    leftNavbar =
      ul [ class "nav navbar-nav" ]
        [ li [] [ navLink Routing.Composers [] [ text "Composers" ] ]
        , li [] [ navLink Routing.NewAssemblage [] [ text "New Assemblage" ]]
        ]
    sessionNav =
      case model.session of
        Just session ->
          li [ class "dropdown" ]
            [ a [ href "#", class "dropdown-toggle", attribute "data-toggle" "dropdown", attribute "role" "button", attribute "aria-haspopup" "true", attribute "aria-expanded" "false" ]
              (faText "user-circle-o" session.user.username)
              , ul [ class "dropdown-menu" ]
              [ li [] [ a [ href "#", onClick (SessionMsg Session.SignOut) ] (faText "sign-out" "Sign Out") ] ]
            ]
        Nothing ->
          li [] [ navLink Routing.NewSession [] (faText "sign-in" "Sign In") ]
    rightNavbar =
      ul [ class "nav navbar-nav navbar-right" ] [ sessionNav ]
  in
    div []
      [ nav [ class "navbar navbar-default" ]
        [ div [ class "container" ]
          [ navbarHeader
          , div [ class "collapse navbar-collapse" ]
            [ leftNavbar
            , rightNavbar
            ]
          ]
        ]
        , div [ class "container" ]
          [ Flash.view model.flash
          , main_ [ attribute "role" "main" ] [ template model ]
          ]
      ]

-- MAIN

processUrl : Navigation.Location -> Msg
processUrl navLoc =
  RoutingMsg (Routing.UpdatePage navLoc)

main : Program Never Model Msg
main =
  Navigation.program processUrl
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
