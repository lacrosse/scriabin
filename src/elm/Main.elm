module Main exposing (..)

import Html exposing (Html, Attribute, text, a,
                      div, span, button, ul, li,
                      main_, nav, h1, h3, h4, p,
                      small, table, thead, tbody,
                      tr, th, td)
import Html.Attributes exposing (class, type_, attribute, href, target)
import Html.Events exposing (onClick, onWithOptions)
import Components.FontAwesome exposing (faText)
import Session
import Components.Flash as Flash
import Navigation
import Routing
import Regex
import Json.Decode
import Store exposing (Store)
import Assemblage exposing (Assemblage)
import Tag exposing (Tag)
import Assembly exposing (Assembly)
import File exposing (File)

main : Program Never Model Msg
main =
  Navigation.program processUrl
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }

-- MODEL

type alias Model =
  { routing : Routing.Model
  , flash : Flash.Model
  , session : Session.Model
  , store : Store.Model
  }

initialModel : Navigation.Location -> Model
initialModel navLocation =
  { routing = Routing.initialModel navLocation
  , session = Session.initialModel
  , flash = Flash.initialModel
  , store = Store.initialModel
  }

init : Navigation.Location -> ( Model, Cmd Msg )
init navLocation =
  ( initialModel navLocation
  , Cmd.none
  )

-- UPDATE

type Msg
  = Noop
  | SessionMsg Session.Msg
  | RoutingMsg Routing.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
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
        (updatedRouting, cmd) = Routing.update msg model.routing
      in
        ( { model | routing = updatedRouting }
        , Cmd.map RoutingMsg cmd
        )

-- VIEW

navLink : Routing.Route -> List (Attribute Msg) -> List (Html Msg) -> Html Msg
navLink page attributes =
  let
    string = Routing.routeToString page
    options = { stopPropagation = True, preventDefault = True }
    decode = Json.Decode.succeed (RoutingMsg (Routing.VisitPage page))
    on = onWithOptions "click" options decode
  in a (href string :: on :: attributes)

template : Model -> List (Html Msg)
template { routing, store } =
  case routing.currentRoute of
    Just Routing.Root ->
      [root]
    Just Routing.NewSession ->
      newSessionView
    Just Routing.Composers ->
      let assemblages = List.filter Assemblage.isComposer store.assemblages
      in assemblagesView assemblages
    Just (Routing.Assemblage id) ->
      case findInList (\a -> a.id == id) store.assemblages of
        Just assemblage ->
          assemblageView assemblage store
        Nothing ->
          [notFound]
    Nothing ->
      [notFound]

root : Html msg
root =
  text "Hi."

notFound : Html msg
notFound =
  div []
    [ h1 [] [ text "Not found" ]
    , p [] [ text "Try again." ]
    ]

newSessionView : List (Html msg)
newSessionView =
  [text "Sign in please"]

assemblagesView : List Assemblage -> List (Html Msg)
assemblagesView list =
  [div [] (List.map assemblageRowView list)]

assemblageLink : Assemblage -> Html Msg
assemblageLink { id, name } =
  navLink (Routing.Assemblage id) [] [ text name ]

assemblageRowView : Assemblage -> Html Msg
assemblageRowView a =
  p [] [ assemblageLink a ]

wikipediaPath : String -> String
wikipediaPath name =
  let base = "https://en.wikipedia.org/wiki/"
  in base ++ Regex.replace Regex.All (Regex.regex " ") (\_ -> "_") name

fileRowView : File -> Html Msg
fileRowView { name } =
  p [] [ text name ]

compositionRowView : Assemblage -> Html Msg
compositionRowView a =
  p [] [ assemblageLink a ]

assemblageView : Assemblage -> Store -> List (Html Msg)
assemblageView assemblage store =
  case assemblage.kind of
    Assemblage.Person ->
      personView assemblage store
    Assemblage.Composition ->
      compositionView assemblage store
    Assemblage.Recording ->
      recordingView assemblage store

assemblageRow : Assemblage -> Html Msg
assemblageRow a =
  tr []
    [ td []
      [ assemblageLink a ]
    ]

assemblageTable : String -> List Assemblage -> List (Html Msg)
assemblageTable name assemblages =
  if List.isEmpty assemblages then
    []
  else
    [ table [ class "table" ]
      [ thead []
        [ tr []
          [ th []
            [ text name ]
          ]
        ]
      , tbody [] (List.map assemblageRow assemblages)
      ]
    ]

fileTable : List File -> List (Html Msg)
fileTable =
  List.map fileRowView

assemblagesThroughAssemblies : Assemblage -> Store -> Assembly.Kind -> Assemblage.Kind -> List Assemblage
assemblagesThroughAssemblies { id } { assemblies, assemblages } kind assemblageKind =
  assemblies
    |> List.filter (\a -> a.childAssemblageId == id && a.kind == kind)
    |> List.map .assemblageId
    |> List.filterMap (findById assemblages)
    |> List.filter (\a -> a.kind == assemblageKind)

childAssemblagesThroughAssemblies : Assemblage -> Store -> Assembly.Kind -> Assemblage.Kind -> List Assemblage
childAssemblagesThroughAssemblies { id } { assemblies, assemblages } kind assemblageKind =
  assemblies
    |> List.filter (\a -> a.assemblageId == id && a.kind == kind)
    |> List.map .childAssemblageId
    |> List.filterMap (findById assemblages)
    |> List.filter (\a -> a.kind == assemblageKind)

prependAndIntersperse : String -> List Assemblage -> List (Html Msg)
prependAndIntersperse string list =
  let
    intersperse list =
      case list of
        [] ->
          []
        [head] ->
          [head]
        [head1, head2] ->
          [head1, text " and ", head2]
        head :: tail ->
          [head, text ", "] ++ (intersperse tail)
    interspersed =
      list
        |> List.map assemblageLink
        |> intersperse
  in text string :: interspersed

composedBy : List Assemblage -> List (Html Msg)
composedBy composers =
  prependAndIntersperse "composed by " composers

reconstructedBy : List Assemblage -> List (Html Msg)
reconstructedBy reconstructors =
  prependAndIntersperse "reconstructed by " reconstructors

personView : Assemblage -> Store -> List (Html Msg)
personView assemblage store =
  let
    header =
      [ h1 [] [ text assemblage.name ]
      , p [] [ a [ href (wikipediaPath assemblage.name), target "_blank" ] [ text "Wikipedia" ] ]
      ]
    files = List.filterMap (findById store.files) assemblage.fileIds
    compositions =
      childAssemblagesThroughAssemblies assemblage store Assembly.Composed Assemblage.Composition
    reconstructions =
      childAssemblagesThroughAssemblies assemblage store Assembly.Reconstructed Assemblage.Composition
  in
    header
    ++ (fileTable files)
    ++ (assemblageTable "Compositions" compositions)
    ++ (assemblageTable "Reconstructions of other composers' works" reconstructions)

compositionHeader : Assemblage -> Store -> Bool -> List (Html Msg)
compositionHeader assemblage store h1Link =
  let
    h1Contents =
      if h1Link then
        [ assemblageLink assemblage ]
      else
        [ text (Assemblage.fullName assemblage) ]
    nameHeader = h1 [] h1Contents
    composers =
      assemblagesThroughAssemblies assemblage store Assembly.Composed Assemblage.Person
    composedByHeader =
      if List.isEmpty composers then
        []
      else
        [h3 [] (composedBy composers)]
    reconstructors =
      assemblagesThroughAssemblies assemblage store Assembly.Reconstructed Assemblage.Person
    reconstructedByHeader =
      if List.isEmpty reconstructors then
        []
      else
        [h4 [] (reconstructedBy reconstructors)]
  in [nameHeader] ++ composedByHeader ++ reconstructedByHeader

tagsRow : List Tag -> List (Html Msg)
tagsRow tags =
  if List.isEmpty tags then
    []
  else
    [ small []
      (
        text "tags: "
        :: (List.map (\t -> span [ class "label label-default" ] [ text t.name ]) tags)
      )
    ]

compositionView : Assemblage -> Store -> List (Html Msg)
compositionView assemblage store =
  let
    header =
      compositionHeader assemblage store False
    tags = []
    recordings =
      childAssemblagesThroughAssemblies assemblage store Assembly.Recorded Assemblage.Recording
    compositionFiles = []
  in
    header
    ++ (tagsRow tags)
    ++ (assemblageTable "Recordings" recordings)
    ++ (fileTable compositionFiles)

recordingView : Assemblage -> Store -> List (Html Msg)
recordingView assemblage store =
  let
    compositions =
      assemblagesThroughAssemblies assemblage store Assembly.Recorded Assemblage.Composition
    header =
      compositions
        |> List.map (\c -> compositionHeader c store True)
        |> List.foldr (++) []
    files = List.filterMap (findById store.files) assemblage.fileIds
  in header ++ (fileTable files)

data : List ( String, String ) -> List (Attribute Msg)
data =
  let mapper (key, value) = attribute ("data-" ++ key) value
  in List.map mapper

aria : List ( String, String ) -> List (Attribute Msg)
aria =
  let mapper (key, value) = attribute ("aria-" ++ key) value
  in List.map mapper

view : Model -> Html Msg
view model =
  let
    threeBars = List.repeat 3 (span [ class "icon-bar" ] [])
    navbarHeader =
      div [ class "navbar-header" ]
        [ button
          (
            [ type_ "button"
            , class "navbar-toggle collapsed"
            ]
            ++ (data [("toggle", "collapse")])
            ++ (aria [("expanded", "false")])
          )
          ( span [ class "sr-only" ] [ text "Toggle navigation" ] :: threeBars )
        , navLink Routing.Root [ class "navbar-brand" ] (faText "music" "Celeste")
        ]
    leftNavbar =
      ul [ class "nav navbar-nav" ]
        [ li []
          [ navLink Routing.Composers [] [ text "Composers" ]
          ]
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
          li []
            [ navLink Routing.NewSession [] (faText "sign-in" "Sign In")
            ]
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
          (
            (Flash.view model.flash)
            ++ [ main_ [ attribute "role" "main" ] (template model) ]
          )
      ]

-- FUNCTIONS

findInList : (a -> Bool) -> List a -> Maybe a
findInList predicate list =
  case list of
    [] ->
      Nothing
    head :: tail ->
      if predicate head then
        Just head
      else
        findInList predicate tail

findById : List { b | id : a } -> a -> Maybe { b | id : a }
findById list fileId =
  findInList (\f -> f.id == fileId) list

processUrl : Navigation.Location -> Msg
processUrl navLoc =
  RoutingMsg (Routing.UpdatePage navLoc)
